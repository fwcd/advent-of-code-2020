program Day12;

{$mode objFPC}

uses
    Sysutils;

type
    Position = record
        x: Integer;
        y: Integer;
    end;
    FacingPosition = record
        position: Position;
        facing: String;
    end;
    
function turnLeft(facing: String): String;
begin
    case facing of
        'N': result := 'W';
        'W': result := 'S';
        'S': result := 'E';
        'E': result := 'N';
    end;
end;

function turnRight(facing: String): String;
begin
    result := turnLeft(turnLeft(turnLeft(facing)));
end;

function turn(facing: String; quarters: Integer): String;
var
    i: Integer;
begin
    result := facing;
    if quarters > 0 then begin
        for i := 1 to quarters do begin
            result := turnRight(result);
        end;
    end else if quarters < 0 then begin
        for i := 1 to -quarters do begin
            result := turnLeft(result);
        end;
    end;
end;

function movePosition(current: Position; facing: String; value: Integer): Position;
begin
    case facing of
        'N': with result do begin
                x := current.x;
                y := current.y - value;
            end;
        'W': with result do begin
                x := current.x - value;
                y := current.y;
            end;
        'S': with result do begin
                x := current.x;
                y := current.y + value;
            end;
        'E': with result do begin
                x := current.x + value;
                y := current.y;
            end;
    end;
end;

function applyAction(current: FacingPosition; raw: String): FacingPosition;
var
    rawValue: String;
    action: String;
    value: Integer;
begin
    rawValue := copy(raw, 2, length(raw));
    action := LeftStr(raw, 1);
    value := StrToInt(rawValue);
    result := current;

    case action of
        'N', 'W', 'S', 'E': result.position := movePosition(current.position, action, value);
        'F': result.position := movePosition(current.position, current.facing, value);
        'L': result.facing := turn(current.facing, -value div 90);
        'R': result.facing := turn(current.facing, value div 90);
    end;
end;

var
    fileIn: TextFile;
    input: String;
    current: FacingPosition;

begin
    with current do begin
        position.x := 0;
        position.y := 0;
        facing := 'E';
    end;
    AssignFile(fileIn, 'resources/input.txt');
    try
        reset(fileIn);
        while not eof(fileIn) do begin
            readln(fileIn, input);
            current := applyAction(current, input);
            writeln('Position: ', current.position.x, ', ', current.position.y, ' > ', current.facing);
        end;
    finally
        CloseFile(fileIn);
    end;
    writeln('Part 1: ', abs(current.position.x) + abs(current.position.y));
end.
