require 'set'

# We use cube coords to identify positions on the hex grid.
# See https://www.redblobgames.com/grids/hexagons/#coordinates
Coords = Struct.new(:x, :y, :z) do
  def +(rhs)
    Coords.new(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z)
  end
end

$dirs = {
  'nw' => Coords.new(0, 1, -1),
  'ne' => Coords.new(1, 0, -1),
  'e'  => Coords.new(1, -1, 0),
  'se' => Coords.new(0, -1, 1),
  'sw' => Coords.new(-1, 0, 1),
  'w'  => Coords.new(-1, 1, 0)
}

def parse_path(s)
  unless s.strip.empty?
    raw = ''
    while not $dirs.key?(raw)
      raw += s[0]
      s = s[1..]
    end
    return $dirs[raw] + parse_path(s)
  else
    return Coords.new(0, 0, 0)
  end
end

black_tiles = Set[]

File.open('resources/input.txt', 'r') do |f|
  f.each_line do |line|
    tile = parse_path(line)
    if black_tiles.include?(tile)
      black_tiles.delete(tile)
    else
      black_tiles.add(tile)
    end
  end
end

puts "Part 1: #{black_tiles.size()}"
