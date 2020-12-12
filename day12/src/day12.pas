program Day12;

{$mode objFPC}

uses
    Sysutils;

type
    Position = record
        x: Integer;
        y: Integer;
    end;
    
function addPositions(p1: Position; p2: Position): Position;
begin
    result.x := p1.x + p2.x;
    result.y := p1.y + p2.y;
end;

function parseDirection(parsedLine: String): Position;
begin
    case parsedLine of
        'N': with result do begin
                x := 0;
                y := -1;
            end;
        'E': with result do begin
                x := -1;
                y := 0;
            end;
        'S': with result do begin
                x := 0;
                y := 1;
            end;
        'W': with result do begin
                x := 1;
                y := 0;
            end;
    end;
end;

var
    fileIn: TextFile;
    input: String;
    currentPos: Position;

begin
    with currentPos do begin
        x := 0;
        y := 0;
    end;
    AssignFile(fileIn, 'resources/input.txt');
    try
        reset(fileIn);
        while not eof(fileIn) do begin
            readln(fileIn, input);
            currentPos := addPositions(currentPos, parseDirection(input));
        end;
    finally
        CloseFile(fileIn);
    end;
    writeln('Part 1: ', currentPos.x + currentPos.y);
end.
