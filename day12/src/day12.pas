program Day12;

{$mode objFPC}

uses
    Sysutils;

var
    fileIn: TextFile;
    input: String;

begin
    AssignFile(fileIn, 'resources/input.txt');
    try
        reset(fileIn);
        while not eof(fileIn) do
        begin
            readln(fileIn, input);
            writeln(input);
        end;
    finally
        CloseFile(fileIn);
    end;
end.
