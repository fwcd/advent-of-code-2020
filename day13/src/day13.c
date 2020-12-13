#include <stdio.h>
#include <stdlib.h>

struct Input {
    int startTs;
    int busses[1024];
    int busCount;
};

int main(void) {
    FILE *file = fopen("resources/input.txt", "r");
    struct Input input = {.startTs = 0, .busses = {0}, .busCount = 0};

    fscanf(file, "%d\n", &input.startTs);

    int i = 0;
    char rawBus[16];

    while (fscanf(file, "%[^,],", rawBus) > 0) {
        char *end = rawBus;
        int bus = (int) strtol(rawBus, &end, 10);
        if (rawBus != end) {
            input.busses[i++] = bus;
        } else {
            input.busses[i++] = -1;
        }
    }

    fclose(file);
    input.busses[i] = 0;
    input.busCount = i;

    return 0;
}
