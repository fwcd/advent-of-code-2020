#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct Input {
    int startTs;
    int busses[1024];
    int busCount;
};

int departureAt(int i, struct Input input) {
    for (int k = 0; k < input.busCount; k++) {
        int bus = input.busses[k];
        if (bus > 0 && i % bus == 0) {
            return bus;
        }
    }
    return 0;
}

int part1(struct Input input) {
    int i = input.startTs;
    while (!departureAt(i, input)) {
        i += 1;
    }
    return (i - input.startTs) * departureAt(i, input);
}

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

    printf("Part 1: %d\n", part1(input));

    return 0;
}
