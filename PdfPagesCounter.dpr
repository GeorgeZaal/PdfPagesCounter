program PdfPagesCounter;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes {для TSTringList}, EsConsole {для русского языка},
  PDFfunc, ShellAPI {для передачи параметра из командной строки};

var
  allfiles, pages : TStringList;
  i, ttl : integer;
  ptch : string;

// Вывод всех файлов в папке:
procedure ShowFiles;
var
  searchResult : TSearchRec;
begin
  allfiles := TStringList.Create;
  if FindFirst({'D:\temp\'}ptch+'*.*', faAnyFile, searchResult) = 0 then
  begin
    repeat
      allfiles.Add({'D:\temp\'}ptch+searchResult.Name);
    until
      FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;
  {чтобы не выводил корневые папки:}
  allfiles.Delete(1);
  allfiles.Delete(0);
end;

procedure ShowPages;
var
  _i : integer;
begin
  pages := TStringList.Create;
  for _i := 0 to allfiles.Count-1 do
    pages.Add(inttostr(GetPdfPageCount(allfiles[_i])));
end;

begin
  if ParamStr(1) = '' then exit else
  ptch := ParamStr(1) + '\';
  ttl := 0;
  ShowFiles;
  ShowPages;
  for i := 0 to allfiles.Count-1 do
    begin
      writeln(allfiles[i]);
      writeln(pages[i]);
      ttl := ttl + strtoint(pages[i]);
    end;
  writeln('---');   
  writeln('Total: ' + inttostr(ttl) + ' pages');
  readln;
end.
