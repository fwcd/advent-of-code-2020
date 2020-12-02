#include <Foundation/Foundation.h>

/// Parses as many characters from the given set as possible and returns the range.
NSRange parse(NSString *s, NSCharacterSet *charSet, int *i) {
    int length = [s length];
    int start = *i;
    for (; *i < length && [charSet characterIsMember:[s characterAtIndex:*i]]; ++*i);
    return NSMakeRange(start, *i - start);
}

/// Parses the given password/rule line and applies the given predicate.
BOOL isValidLine(NSString *line, BOOL (*predicate)(int, int, char, NSString *)) {
    if ([[line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] length] > 0) {
        int parsePos = 0;
        int first = [[line substringWithRange:parse(line, [NSCharacterSet decimalDigitCharacterSet], &parsePos)] intValue];
        parse(line, [NSCharacterSet punctuationCharacterSet], &parsePos);
        int second = [[line substringWithRange:parse(line, [NSCharacterSet decimalDigitCharacterSet], &parsePos)] intValue];
        parse(line, [NSCharacterSet whitespaceCharacterSet], &parsePos);
        char character = [[line substringWithRange:parse(line, [NSCharacterSet letterCharacterSet], &parsePos)] characterAtIndex:0];
        parse(line, [[NSCharacterSet letterCharacterSet] invertedSet], &parsePos);
        NSString *password = [line substringWithRange:parse(line, [NSCharacterSet letterCharacterSet], &parsePos)];

        return predicate(first, second, character, password);
    } else {
        return NO;
    }
}

BOOL part1Predicate(int min, int max, char c, NSString *password) {
    int length = [password length];
    int occurrences = 0;
    int i;
    for (i = 0; i < length; i++) {
        if (c == [password characterAtIndex:i]) {
            occurrences++;
        }
    }

    return occurrences >= min && occurrences <= max;
}

BOOL part2Predicate(int first, int second, char c, NSString *password) {
    return ([password characterAtIndex:first - 1] == c) ^ ([password characterAtIndex:second - 1] == c);
}

int validLineCount(NSArray *lines, BOOL (*predicate)(int, int, char, NSString *)) {
    int validLines = 0;
    int lineCount = [lines count];
    int i;

    for (i = 0; i < lineCount; i++) {
        if (isValidLine([lines objectAtIndex:i], predicate)) {
            validLines++;
        }
    }

    return validLines;
}

int main(void) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSError *error = nil;
    NSString *input = [[[NSString alloc] initWithContentsOfFile:@"resources/input.txt"
                                                       encoding:NSUTF8StringEncoding
                                                          error:&error] autorelease];
    if (error) {
        NSLog(@"Error while opening file: %@", error);
        return 1;
    }

    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    NSLog(@"Part 1: %d", validLineCount(lines, part1Predicate));
    NSLog(@"Part 2: %d", validLineCount(lines, part2Predicate));

    [pool drain];
    return 0;
}
