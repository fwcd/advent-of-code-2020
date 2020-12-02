#include <Foundation/Foundation.h>

NSRange parse(NSString *s, NSCharacterSet *charSet, int *i) {
    int length = [s length];
    int start = *i;
    for (; *i < length && [charSet characterIsMember:[s characterAtIndex:*i]]; ++*i);
    return NSMakeRange(start, *i - start);
}

BOOL isValidLine(NSString *line) {
    if ([[line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] length] > 0) {
        int parsePos = 0;
        int minOccurrences = [[line substringWithRange:parse(line, [NSCharacterSet decimalDigitCharacterSet], &parsePos)] intValue];
        parse(line, [NSCharacterSet punctuationCharacterSet], &parsePos);
        int maxOccurrences = [[line substringWithRange:parse(line, [NSCharacterSet decimalDigitCharacterSet], &parsePos)] intValue];
        parse(line, [NSCharacterSet whitespaceCharacterSet], &parsePos);
        char character = [[line substringWithRange:parse(line, [NSCharacterSet letterCharacterSet], &parsePos)] characterAtIndex:0];
        parse(line, [[NSCharacterSet letterCharacterSet] invertedSet], &parsePos);
        NSString *password = [line substringWithRange:parse(line, [NSCharacterSet letterCharacterSet], &parsePos)];
        NSLog(@"min: %d, max: %d, char: %c, pw: %@", minOccurrences, maxOccurrences, character, password);
        int length = [password length];

        int occurrences = 0;
        int i;
        for (i = 0; i < length; i++) {
            if (character == [password characterAtIndex:i]) {
                occurrences++;
            }
        }

        BOOL valid = occurrences >= minOccurrences && occurrences <= maxOccurrences;
        return valid;
    } else {
        return NO;
    }
}

int part1(NSArray *lines) {
    int validLines = 0;
    int lineCount = [lines count];
    int i;

    for (i = 0; i < lineCount; i++) {
        if (isValidLine([lines objectAtIndex:i])) {
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
    NSLog(@"Part 1: %d", part1(lines));

    [pool drain];
    return 0;
}
