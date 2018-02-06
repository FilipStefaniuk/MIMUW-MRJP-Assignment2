#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void printInt(int x) {
    printf("%d\n", x);
}

void printString(char* string) {
    printf("%s\n", string);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

int readInt() {
    int x;
    scanf("%d\n", &x);
    return x;
}

char* readString() {
 	char* line = NULL;
	size_t len = 0;
	size_t read;
	if ((read = getline(&line, &len, stdin)) != -1) {
	  char* cleanedLine = (char*) malloc(sizeof(char)*(strlen(line)));
	  for (int i = 0; i < strlen(line); i++) {
	    cleanedLine[i] = line[i];
	  }
	  cleanedLine[strlen(line) - 1] = '\0';
		return cleanedLine;
	} else {
		error();
		return NULL;
	}   
}

char* concat(char* s1, char* s2) {
    char* t = (char*)malloc(strlen(s1) + strlen(s2) + 1);
    return strcat(strcpy(t,s1), s2);
}
