import 'dart:io';

Future<void> main(List<String> arguments) async {
  final input = await File('resources/input.txt').readAsString();
  print(input);
}
