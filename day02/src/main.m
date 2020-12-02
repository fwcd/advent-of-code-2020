#include <Foundation/Foundation.h>

int main(void) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSObject *test = [[NSObject alloc] init];
    printf("Test\n");
    [pool drain];
    return 0;
}
