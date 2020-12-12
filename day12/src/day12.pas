program Day12;

{$mode objFPC}

uses
    Sysutils;

type
    Instruction = record
        action: String;
        value: Integer;
    end;
    Position = record
        x: Integer;
        y: Integer;
    end;
    FacingPosition = record
        position: Position;
        facing: String;
    end;
    WaypointPosition = record
        position: Position;
        waypoint: Position;
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

function rotatePosition(position: Position; quarters: Integer): Position;
begin
    case turn('N', quarters) of
        'N': result := position;
        'W': with result do begin
                x := -position.y;
                y := position.x;
            end;
        'S': with result do begin
                x := -position.x;
                y := -position.y;
            end;
        'E': with result do begin
                x := position.y;
                y := -position.x;
            end;
    end;
end;

function addPosition(a: Position; b: Position): Position;
begin
    with result do begin
        x := a.x + b.x;
        y := a.y + b.y;
    end;
end;

function scalePosition(a: Position; b: Integer): Position;
begin
    with result do begin
        x := a.x * b;
        y := a.y * b;
    end;
end;

function manhattanAbs(p: Position): Integer;
begin
    result := abs(p.x) + abs(p.y);
end;

function parseInstruction(raw: String): Instruction;
var
    rawValue: String;
begin
    rawValue := copy(raw, 2, length(raw));
    with result do begin
        action := LeftStr(raw, 1);
        value := StrToInt(rawValue);
    end;
end;

function applyPart1Instruction(current: FacingPosition; inst: Instruction): FacingPosition;
begin
    result := current;
    case inst.action of
        'N', 'W', 'S', 'E': result.position := movePosition(current.position, inst.action, inst.value);
        'F': result.position := movePosition(current.position, current.facing, inst.value);
        'L': result.facing := turn(current.facing, -inst.value div 90);
        'R': result.facing := turn(current.facing, inst.value div 90);
    end;
end;

function applyPart2Instruction(current: WaypointPosition; inst: Instruction): WaypointPosition;
begin
    result := current;
    case inst.action of
        'N', 'W', 'S', 'E': result.waypoint := movePosition(current.waypoint, inst.action, inst.value);
        'F': result.position := addPosition(current.position, scalePosition(current.waypoint, inst.value));
        'L': result.waypoint := rotatePosition(current.waypoint, -inst.value div 90);
        'R': result.waypoint := rotatePosition(current.waypoint, inst.value div 90);
    end;
end;

var
    fileIn: TextFile;
    input: String;
    inst: Instruction;
    part1Current: FacingPosition;
    part2Current: WaypointPosition;

begin
    with part1Current do begin
        position.x := 0;
        position.y := 0;
        facing := 'E';
    end;
    with part2Current do begin
        position.x := 0;
        position.y := 0;
        waypoint.x := -10;
        waypoint.y := -1;
    end;
    AssignFile(fileIn, 'resources/input.txt');
    try
        reset(fileIn);
        while not eof(fileIn) do begin
            readln(fileIn, input);
            inst := parseInstruction(input);
            part1Current := applyPart1Instruction(part1Current, inst);
            part2Current := applyPart2Instruction(part2Current, inst);
            writeln('pos x: ', part2Current.position.x, ', y: ', part2Current.position.y, ' - wp x: ', part2Current.waypoint.x, ', y: ', part2Current.waypoint.y);
        end;
    finally
        CloseFile(fileIn);
    end;
    writeln('Part 1: ', manhattanAbs(part1Current.position));
    writeln('Part 2: ', manhattanAbs(part2Current.position));
end.
