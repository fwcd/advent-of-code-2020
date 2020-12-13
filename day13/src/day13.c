#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *file = fopen("resources/input.txt", "r");
    int startTs;
    int busses[1024] = {0};
    int i = 0;
    fscanf(file, "%d\n", &startTs);

    char rawBus[16];
    while (fscanf(file, "%[^,],", rawBus) > 0) {
        char *end = rawBus;
        int bus = (int) strtol(rawBus, &end, 10);
        if (rawBus != end) {
            busses[i++] = bus;
        } else {
            busses[i++] = -1;
        }
    }

    busses[i] = 0;
    fclose(file);
    printf("Read %d numbers: ", i);
    int *bus = busses;
    while (*bus) {
        printf("%d, ", *bus);
        bus++;
    }
    return 0;
}
