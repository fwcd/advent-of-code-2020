#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *file = fopen("resources/input.txt", "r");
    char rawNum[1024] = {0};
    while (fscanf(file, "%s\n", rawNum) > 0) {
        printf("Line: %s\n", rawNum);
    }
    fclose(file);
}
