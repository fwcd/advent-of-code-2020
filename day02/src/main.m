#include <Foundation/Foundation.h>

int main(void) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    Test *t = [[[Test alloc] init] autorelease];
    [pool drain];
    return 0;
}
