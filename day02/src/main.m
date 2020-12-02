#include <Foundation/Foundation.h>
#include <Foundation/NSRegularExpression.h>

BOOL isValidLine(NSString *line) {
    NSError *error = nil;
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"z"
                                                                           options:0
                                                                             error:&error];
    if (error) {
        NSLog(@"Could not create regex!");
        return NO;
    }

    NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];

    if (match) {
        NSLog(@"Matched: %@", line);
        NSLog(@"Range %@", [match range]);
        int minOccurrences = [[line substringWithRange:[match rangeAtIndex:1]] intValue];
        int maxOccurrences = [[line substringWithRange:[match rangeAtIndex:2]] intValue];
        char character = [[line substringWithRange:[match rangeAtIndex:3]] characterAtIndex:0];
        NSString *password = [line substringWithRange:[match rangeAtIndex:4]];
        int length = [password length];

        int occurrences = 0;
        int i;
        for (i = 0; i < length; i++) {
            if (character == [password characterAtIndex:i]) {
                occurrences++;
            }
        }

        BOOL valid = occurrences >= minOccurrences && occurrences <= maxOccurrences;
        NSLog(@"Line valid: %@", valid);
        return valid;
    } else {
        return NO;
    }
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

    int validLines = 0;
    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    int lineCount = [lines count];
    int i;
    for (i = 0; i < lineCount; i++) {
        if (isValidLine([lines objectAtIndex:i])) {
            validLines++;
        }
    }

    NSLog(@"Valid lines: %@", validLines);

    [pool drain];
    return 0;
}
