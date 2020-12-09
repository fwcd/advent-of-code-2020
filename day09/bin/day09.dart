import 'dart:io';

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

Future<void> main(List<String> arguments) async {
  final input = await File('resources/input.txt').readAsString();
  final xs = input.split('\n').map((l) => int.tryParse(l)).where((x) => x != null).toList();
  final n = 25;
  final part1Index = List<int>.generate(xs.length - n, (i) => i + n)
    .firstWhere((i) => !isXMAS(xs, i, n));
  final part1 = xs[part1Index];
  print('Part 1: $part1 at $part1Index');
}
