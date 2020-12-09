import 'dart:io';

import 'dart:math';

bool isXMAS(List<int> xs, int i, int n) {
  for (final x in xs.getRange(i - n, i)) {
    for (final y in xs.getRange(i - n, i)) {
      if (x != y && (xs[i] == x + y)) {
        return true;
      }
    }
  }
  return false;
}

List<int> contiguousSum(List<int> xs, int i, int expected) {
  var j = i + 1;
  var sum = xs[i];
  while (j < xs.length && sum < expected) {
    sum += xs[j];
    j++;
  }
  return sum == expected ? xs.getRange(i, j).toList() : null;
}

Future<void> main(List<String> arguments) async {
  final input = await File('resources/input.txt').readAsString();
  final xs = input.split('\n').map((l) => int.tryParse(l)).where((x) => x != null).toList();
  final n = 25;
  final part1Index = List<int>.generate(xs.length - n, (i) => i + n)
    .firstWhere((i) => !isXMAS(xs, i, n));
  final part1 = xs[part1Index];
  print('Part 1: $part1 at $part1Index');

  final part2Cont = List<int>.generate(xs.length, (i) => i)
    .map((i) => contiguousSum(xs, i, part1))
    .firstWhere((x) => x != null);
  final part2 = part2Cont.reduce(min) + part2Cont.reduce(max);
  print('Part 2: $part2 from $part2Cont');
}
