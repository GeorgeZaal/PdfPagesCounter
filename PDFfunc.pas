unit PDFfunc;

 interface 

 uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, zlib;

type
  PPdfObj = ^TPdfObj;
  TPdfObj = record
    number,
    offset: integer;
    filePtr: PAnsiChar;
    stmObjNum: integer;
  end;

function GetPdfPageCount(const filename: string): integer;

implementation

function ReversePngFilter(var buffer: PAnsiChar;
  row_size, buffer_cnt: integer): integer;
var
  i: integer;
  p, end_buff: PAnsiChar;
  filterType: AnsiChar;

  function paeth(c, b, a: byte): byte;
  var
    p,pa,pb,pc: byte;
  begin
    //a = left, b = above, c = upper left
    p := (a + b - c) and $FF;
    pa := abs(p - a);
    pb := abs(p - b);
    pc := abs(p - c);
    if (pa <= pb) and (pa <= pc) then result := a
    else if pb <= pc then result := b
    else result := c;
  end;

  function Average(a, b: byte): byte;
  begin
    //a = left, b = above
    result := (a + b) shr 1;
  end;

begin
  //result will be new buffer length ...
  result := buffer_cnt;
  p := buffer;
  end_buff := buffer + buffer_cnt;
  while p < end_buff do
  begin
    filterType := p^;
    dec(result);
    dec(end_buff);
    move((p +1)^, p^, end_buff - p);
    case filterType of
      #0:
        ;//no filtering used for this row
      #1:
        begin
          //'sub' filtering
          for i := 1 to row_size -1 do
            (p+i)^ := AnsiChar((ord((p+i)^) + ord((p+i -1)^)) and $FF);
        end;
      #2:
        begin
          //'up' filtering
          if p > buffer then //leave top row alone
            for i := 0 to row_size -1 do
              (p+i)^ := AnsiChar((ord((p+i)^) +
                ord((p+i - row_size)^)) and $FF);
        end;
      #3:
        begin
          //'average' filtering
          if p = buffer then //ie top row
            for i := 1 to row_size -1 do
              (p+i)^ := AnsiChar((ord((p+i)^) +
                average(ord((p+i -1)^), 0)) and $FF)
          else
          begin
            p^ := AnsiChar((ord((p)^) +
              average(0, ord((p - row_size)^))) and $FF);
            for i := 1 to row_size -1 do
              (p+i)^ := AnsiChar((ord((p+i)^) + average(ord((p+i -1)^),
                ord((p+i - row_size)^))) and $FF);
          end;
        end;
      #4:
        begin
          //'paeth' filtering
          if p = buffer then //ie top row
            for i := 1 to row_size -1 do
              (p+i)^ := AnsiChar((ord((p+i)^) +
                paeth(ord((p+i -1)^), 0, 0)) and $FF)
          else
          begin
            p^ := AnsiChar((ord((p)^) +
              paeth(0, ord((p - row_size)^), 0)) and $FF);
            for i := 1 to row_size -1 do
              (p+i)^ := AnsiChar((ord((p+i)^) +
                paeth(ord((p+i -1)^), ord((p+i - row_size)^),
                ord((p+i - row_size -1)^))) and $FF);
          end;
        end;
    end;
    inc(p, row_size);
  end;
  ReallocMem(buffer, result);
end;
//------------------------------------------------------------------------------

function GetPdfPageCount(const filename: string): integer;
var
  ms: TMemoryStream;
  i, j, k, cnt, pagesNum, rootNum, predictor, filterColCnt: integer;
  indexArray: array of integer;
  w1,w2,w3: integer;
  p, pEnd, pSaved, pTmp2, buffer, buffPtr: PAnsiChar;
  PdfObj: PPdfObj;
  PdfObjList: TList;

////////////////////////////////////////////////////////////////////////////////
// Summary of steps taken to parse a PDF doc for its page count :-              
// (Who'd have thought it'd take about 1000 lines of code to do that!?)  
////////////////////////////////////////////////////////////////////////////////

//1.  See if there's a 'Linearization dictionary' for easy parsing.
//    Mostly there isn't so ...
//2.  Locate 'startxref' at end of file
//3.  get 'xref' offset and go to xref table
//4.  depending on version the xref table may or may not be in a compressed
//    stream. If it's in a compressed stream (PDF ver 1.5+) then getting the
//    page number requires a LOT of code which is too convoluted to summarise
//    here. Otherwise it still requires a moderate amount of code ...
//5.  parse the xref table and fill a list with object numbers and offsets
//6.  handle subsections within xref table.
//7.  read 'trailer' section at end of each xref
//8.  store 'Root' object number if found in 'trailer'
//9.  if 'Prev' xref found in 'trailer' - loop back to step 3
//10. locate Root in the object list
//11. locate 'Pages' object from Root
//12. get Count from Pages.

  function GetNumber(out num: integer): boolean;
  var
    tmpStr: string;
  begin
    tmpStr := '';
    while p^ < #33 do inc(p); //skip leading CR,LF & SPC
    while (p^ in ['0'..'9']) do
    begin
      tmpStr := tmpStr + Char(PAnsiChar(p)^);
      inc(p);
    end;
    result := tmpStr <> '';
    if not result then exit;
    num := strtoint(tmpStr);
  end;

  function IsString(const str: ansistring): boolean;
  var
    len: integer;
  begin
    len := length(str);
    result := CompareMem( p, PAnsiChar(str), len);
    if result then inc(p, len);
  end;

  function FindStrInDict(const str: ansistring): boolean;
  var
    nestLvl: integer;
    str1: AnsiChar;
  begin
    //nb: PDF 'dictionaries' start with '<<' and terminate with '>>'
    result := false;
    nestLvl := 0;
    str1 := str[1];
    while not result do
    begin
      while not (p^ in ['>','<',str1]) do inc(p);
      if (p^ = '<') then
      begin
        if (p+1)^ = '<' then begin inc(nestLvl); inc(p); end;
      end
      else if (p^ = '>') then
      begin
        if (p+1)^ = '>' then
        begin
          dec(nestLvl);
          inc(p);
          if nestLvl <= 0 then exit;
        end
      end else
      begin
        result := (nestLvl < 2) and IsString(str);
        if result then exit;
      end;
      inc(p);
    end;
  end;

  function FindEndOfDict: boolean;
  var
    nestLvl: integer;
  begin
    result := true;
    nestLvl := 1;
    while result do
    begin
      while not (p^ in ['>','<']) do inc(p);
      if (p^ = '<') then
      begin
        if (p+1)^ = '<' then begin inc(nestLvl); inc(p); end;
      end
      else if (p+1)^ = '>' then
      begin
        dec(nestLvl);
        if nestLvl < 0 then
          result := false
        else if nestLvl = 0 then
        begin
          inc(p, 2);
          exit; //found end of Dictionary
        end;
        inc(p); //skips first '>'
      end;
      inc(p);
    end;
    result := false;
  end;

  procedure SkipBlankSpace;
  begin
    while (p < pEnd) and (p^ < #33) do inc(p);
  end;

  function GotoObject(objNum: integer): boolean;
  var
    i: integer;
  begin
    i := 0;
    while i < PdfObjList.Count do
      if PPdfObj(PdfObjList[i]).number = objNum then break else inc(i);
    result :=  (i < PdfObjList.Count) and
      (PPdfObj(PdfObjList[i]).filePtr <> nil);
    if not result then exit;
    p := PPdfObj(PdfObjList[i]).filePtr;
    result := GetNumber(i) and (i = objNum);
  end;

  function IsCompressedObject(objNum: integer): boolean;
  var
    i: integer;
  begin
    i := 0;
    while i < PdfObjList.Count do
      if PPdfObj(PdfObjList[i]).number = objNum then break else inc(i);
    result :=  (i < PdfObjList.Count) and
      (PPdfObj(PdfObjList[i]).filePtr = nil);
  end;

  function GetCompressedObjectBuffer(objNum: integer;
    out buffer: PAnsiChar; out bufSize: integer): boolean;
  var
    i, N, First, objIdx: integer;
  begin
    result := false;
    i := 0;
    while i < PdfObjList.Count do
      if PPdfObj(PdfObjList[i]).number = objNum then break else inc(i);
    if (i = PdfObjList.Count) or
      (PPdfObj(PdfObjList[i]).filePtr <> nil) then exit;
    if not GotoObject(PPdfObj(PdfObjList[i]).stmObjNum) then exit;
    objIdx := PPdfObj(PdfObjList[i]).offset;

    pSaved := p;
    if not FindStrInDict('/Type') then exit;
    SkipBlankSpace;
    if not IsString('/ObjStm') then exit;

    p := pSaved;
    //todo - handle uncompressed streams too
    //assume all streams are compressed (though this is really optional)
    if not FindStrInDict('/Filter') then exit;
    SkipBlankSpace;
    //check that this a compression type that we can handle ...
    if not IsString('/FlateDecode') then exit;
    p := pSaved;
    if not FindStrInDict('/DecodeParms') or not
      FindStrInDict('/Columns') or not GetNumber(filterColCnt) then
        filterColCnt := 0; //j = column count (bytes per row)
    if filterColCnt > 0 then
    begin
      SkipBlankSpace;
      if not IsString('/Predictor') or not GetNumber(predictor) then
        predictor := 0;
    end;

    p := pSaved;
    if not FindStrInDict('/N') then exit;
    //N = number of compressed objects in stream ...
    if not GetNumber(N) or (objIdx > N) then exit;

    p := pSaved;
    if not FindStrInDict('/First') then exit;
    if not GetNumber(First) then exit; //First = first object in stream

    p := pSaved;
    if not FindStrInDict('/Length') then exit;
    if not GetNumber(k) then exit; //k = length of compressed stream
    if not FindEndOfDict then exit;
    while (p^ <> 's') do inc(p);
    if not IsString('stream') then exit;
    SkipBlankSpace;

    //decompress the stream ...
    buffer := nil;

    //nb: I'm not sure in which Delphi version these functions were renamed.
    {$IFNDEF UNICODE}
    zlib.DecompressBuf(p, k, k*3, pointer(buffer), bufSize);
    {$ELSE}
    zlib.ZDecompress(p, k, pointer(buffer), bufSize);
    {$ENDIF}

    if (filterColCnt > 0) and (predictor > 9) then
      bufSize := ReversePngFilter(buffer, filterColCnt, bufSize);

    p := buffer;
    for i := 0 to objIdx -1 do
    begin
      if not GetNumber(k) then exit;
      if not GetNumber(k) then exit;
    end;
    if not GetNumber(k) or (k <> objNum) then exit; //check it's the right obj
    if not GetNumber(k) then exit; //k = offset of obj from first obj offset
    if objIdx = N then
    begin
      p := buffer + First + k;
      bufSize := buffer + bufSize - p;
    end else
    begin
      if not GetNumber(i) then exit;
      if not GetNumber(i) then exit;
      p := buffer + First + k;
      bufSize := (i - k);
    end;
    move(p^, buffer^, bufSize);
    ReallocMem(buffer, bufSize);
    p := buffer;
    result := bufSize > 0;
  end;

  function Find(const str: ansistring; search_forwards: boolean = true): boolean;
  var
    len: integer;
    c: AnsiChar;
  begin
    result := false;
    len := length(str);
    if search_forwards then
    begin
      c := str[1];
      repeat
        while (p < pEnd)  and (p^ <> c) do inc(p);
        if (p = pEnd) then exit;
        if StrLComp(p, PAnsiChar(str), len) = 0 then break;
        inc(p);
      until false;
      inc(p,len);
    end else
    begin
      c := str[len];
      repeat
        while (p > PAnsiChar(ms.Memory))  and (p^ <> c) do dec(p);
        if (p = PAnsiChar(ms.Memory)) then exit;
        if StrLComp( (p-len+1), PAnsiChar(str), len) = 0 then break;
        dec(p);
      until false;
      inc(p);
    end;
    result := true;
  end;

  function PAnsiCharToInt(buffer: PAnsiChar; byteCnt: integer): integer;
  begin
    case byteCnt of
      1: result := ord(buffer^);
      2: result := ord(buffer^) shl 8 + ord((buffer+1)^);
      3: result := ord(buffer^) shl 16 +
        ord((buffer+1)^) shl 8 + ord((buffer+2)^);
      4: result := ord(buffer^) shl 24 +
        ord((buffer+1)^) shl 16 + ord((buffer+2)^) +
        ord((buffer+3)^);
      else result := 0;
    end;
  end;

  function GetLinearizedPageNum(out pageNum: integer): boolean;
  var
    pStart,pStop: PAnsiChar;
  begin
    pageNum := -1;
    result := false;
    pStop := p + 32;
    while (p < pStop) and (p^ <> 'o') do inc(p);
    if StrLComp( p, 'obj', 3) <> 0 then exit;
    pStart := p;
    if not FindStrInDict('/Linearized') then exit;
    p := pStart;
    if FindStrInDict('/N ') and GetNumber(pageNum) then result := true;
  end;

begin
  //on error return -1 as page count
  result := -1;
  try
    ms := TMemoryStream.Create;
    PdfObjList := TList.Create;
    try
      ms.LoadFromFile(filename);

      p := PAnsiChar(ms.Memory);
      pEnd := PAnsiChar(ms.Memory) + ms.Size;

      //for an easy life let's hope the file has a 'linearization dictionary'
      //at the beginning of the document ...
      if GetLinearizedPageNum(result) then exit;

      //find 'startxref' ignoring '%%EOF' at end of file
      p := pEnd -5;
      if not Find('startxref', false) then exit;

      rootNum := -1; //ie flag as not yet found

      if not GetNumber(k) then exit;  //xref offset ==> k
      p :=  PAnsiChar(ms.Memory) + k;

      if StrLComp(p, 'xref', 4) <> 0 then
      begin
        //most probably a cross-reference stream (ie PDF doc ver 1.5+)
        //so LOTS of hard work to do now ...

        if not GetNumber(k) then exit; //stream obj number
        if not GetNumber(k) then exit; //stream obj revision number

        pSaved := p;
        if not FindStrInDict('/Type') then exit;
        SkipBlankSpace;
        if not IsString('/XRef') then exit;

        //todo - check for and manage /Prev too

        p := pSaved;
        if not FindStrInDict('/Root') then exit;
        SkipBlankSpace;
        if not GetNumber(rootNum) then exit;
        p := pSaved;

        //todo - handle uncompressed streams too

        //assume all streams are compressed (though this is really optional)
        if not FindStrInDict('/Filter') then exit;
        SkipBlankSpace;
        //check that this a compression type that we can handle ...
        if not IsString('/FlateDecode') then exit;
        p := pSaved;
        if not FindStrInDict('/DecodeParms') or not
          FindStrInDict('/Columns') or not GetNumber(filterColCnt) then
            filterColCnt := 0; //j = column count (bytes per row)
        if filterColCnt > 0 then
        begin
          SkipBlankSpace;
          if not IsString('/Predictor') or not GetNumber(predictor) then
            predictor := 0; //Filtering used prior to Flate (PNG filter >= 10)
        end;

        //get the stream cross-ref table field sizes ...
        p := pSaved;
        if not FindStrInDict('/W') then exit;
        SkipBlankSpace;
        if p^ <> '[' then exit;
        inc(p);
        if not GetNumber(w1) or (w1 <> 1) or not GetNumber(w2) or
          not GetNumber(w3) then exit;

        //Index [F1 N1, ..., Fn, Nn]. If absent assumes F1 = 0 & N based on size
        //(Fn: first object in table subsection; Nn: number in table subsection)
        indexArray := nil;
        p := pSaved;
        if FindStrInDict('/Index') then
        begin
          SkipBlankSpace;
          if p^ <> '[' then exit;
          inc(p);
          while true do
          begin
            k := length(indexArray);
            SetLength(indexArray, k +2);
            if not GetNumber(indexArray[k]) or
              not GetNumber(indexArray[k+1]) then exit;
            pTmp2 := p;
            if not GetNumber(i) then break;
            p := pTmp2;
          end;
        end;

        p := pSaved;
        if not FindStrInDict('/Length') then exit;
        if not GetNumber(k) then exit; //k = length of compressed stream
        if not FindEndOfDict then exit;
        while (p^ <> 's') do inc(p);
        if not IsString('stream') then exit;
        SkipBlankSpace;

        //decompress the stream ...
        buffer := nil;
        try
          {$IFNDEF UNICODE}
          zlib.DecompressBuf(p, k, k*3, pointer(buffer), cnt);
          {$ELSE}
          zlib.ZDecompress(p, k, pointer(buffer), cnt);
          {$ENDIF}

          //now de-filter the decompressed output (typically PNG filtering)
          //Filter Columns should match the byte count of /W[X Y Z]
          //The decompressed stream prefiltered size == (X Y Z +1) * entries
          //(ie allowing extra byte at the start of each column for filter type)
          //see also http://www.w3.org/TR/PNG-Filters.html

          if (filterColCnt > 0) and (predictor > 9) then
            cnt := ReversePngFilter(buffer, filterColCnt, cnt);

          //make sure the decompressed & defiltered table is a valid size ...
          if cnt mod (w1 + w2 + w3) <> 0 then exit;

          //if the Index array is empty then use the default values ...
          if length(indexArray) = 0 then
          begin
            setLength(indexArray, 2);
            indexArray[0] := 0;
            indexArray[1] := cnt div (w1 + w2 + w3);
          end;

          buffPtr := buffer;
          //loop through each subsection in the table and populate our
          //object list ...
          for i := 1 to (length(indexArray) div 2) do
          begin
            k := indexArray[i*2 -2]; //k := base object number
            for j := 1 to indexArray[i*2 -1] do
            begin
              case buffPtr^ of
                #0: ;//ignore (free object)

                #1: //uncompressed object
                  begin
                    inc(buffPtr);
                    new(PdfObj);
                    PdfObjList.Add(PdfObj);
                    PdfObj.number := k;
                    PdfObj.stmObjNum := -1;
                    PdfObj.offset := PAnsiCharToInt(buffPtr, w2);
                    PdfObj.filePtr := PAnsiChar(ms.Memory) + PdfObj.offset;
                    dec(buffPtr, w1);
                  end;
                #2: //compressed object
                  begin
                    inc(buffPtr, w1);
                    new(PdfObj);
                    PdfObjList.Add(PdfObj);
                    PdfObj.number := k;
                    PdfObj.stmObjNum := PAnsiCharToInt(buffPtr, w2);
                    inc(buffPtr,w2);
                    PdfObj.offset := PAnsiCharToInt(buffPtr, w3);
                    PdfObj.filePtr := nil;
                    dec(buffPtr, w1+w2);
                  end;
              end;
              inc(buffPtr, w1 + w2 + w3);
              inc(k);
            end;
          end;
        finally
          if assigned(buffer) then FreeMem(buffer);
        end;

        if rootNum < 0 then exit;
        if not GotoObject(rootNum) then exit;

        if not FindStrInDict('/Pages') then exit;
        //get the Pages' object number, go to it and get the page count ...
        if not GetNumber(pagesNum) then exit;
        k := -1;
        if IsCompressedObject(pagesNum) then
        begin
          if not GetCompressedObjectBuffer(pagesNum, buffer, cnt) then exit;
          try
            if not FindStrInDict('/Count') or not GetNumber(k) then exit;
          finally
            FreeMem(buffer);
          end;
        end
        else if not GotoObject(pagesNum) or
          not FindStrInDict('/Count') or not GetNumber(k) then exit;

        //if we get this far the page number has been FOUND!!!
        result := k;
        exit;
      end;

      //OK, most likely a PDF doc ver 1.4 (or earlier) ...

      inc(p,4); //ie skip over 'xref'

      while true do //top of loop  //////////////////////////////
      begin
        //get base object number ==> k
        if not GetNumber(k) then exit;
        //get object count ==> cnt
        if not GetNumber(cnt) then exit;
        //it is possible to have 0 objects in a section
        SkipBlankSpace;

        //add all objects in section to list ...
        for cnt := 0 to cnt-1 do
        begin
          new(PdfObj);
          PdfObjList.Add(PdfObj);
          PdfObj.number := k + cnt;
          if not GetNumber(PdfObj.offset) then exit;
          PdfObj.filePtr := PAnsiChar(ms.Memory) + PdfObj.offset;
          //while each entry SHOULD be exactly 20 bytes, not all PDF document
          //creators seems to adhere to this :( ...
          while not (p^ in [#10,#13]) do inc(p);
          while (p^ in [#10,#13]) do inc(p);
        end;
        //check for and process further subsections ...
        if p^ in ['0'..'9'] then continue;

        // parse 'trailer dictionary' ...
        if not IsString('trailer') then exit;
        pSaved := p;
        // get Root (aka Catalog) ...
        if (rootNum = -1) and FindStrInDict('/Root') then
          if not GetNumber(rootNum) then exit;
        p := pSaved;
        if not FindStrInDict('/Prev') then break; //no more xrefs

        //next xref offset ==> k
        if not GetNumber(k) then exit;
        p :=  PAnsiChar(ms.Memory) + k +4;

      end; //bottom of loop /////////////////////////////////////

      //Make sure we've got Root's object number ...
      if rootNum < 0 then exit;
      if not GotoObject(rootNum) then exit;

      if not FindStrInDict('/Pages') then exit;
      //get Pages object number then goto pagesNum ...
      if not GetNumber(pagesNum) or not GotoObject(pagesNum) then exit;
      if not FindStrInDict('/Count') then exit;
      if not GetNumber(cnt) then exit;
      //occasionally the 'count' value is an indirect object
      if GetNumber(k) and IsString(' R') then
      begin
        if not GotoObject(cnt) then exit;
        if not GetNumber(k) or //skip the generation num
          not IsString(' obj') or
          not GetNumber(cnt) then exit;
      end;
      result := cnt; //FOUND!!!!!!

    finally
      for k := 0 to PdfObjList.Count -1 do
        dispose(PPdfObj(PdfObjList[k]));
      PdfObjList.Free;
      ms.Free;
    end;
  except
    //nb: errors are flagged by returning -1
  end;
end;

end.