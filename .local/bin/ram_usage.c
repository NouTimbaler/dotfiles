#include <stdio.h>
#include <stdlib.h>


#define BUF_SIZE 50



void truncate_spaces(char *str) {
  int src = 0, dst = 0, index, i = -1;
  
  while(*(str + dst) == ' ') dst++;

    while(*(str + dst) != '\0') {
        *(str + src) = *(str + dst);
        if(*(str + (dst++)) == ' ')
            while(*(str + dst) == ' ') dst++;

        src++;
    }

    *(str + src) = '\0';

    i = 0;
    while(str[i] != '\0'){
        if(str[i] != ' ' && str[i] != '\t' && str[i] != '\n'){
            index= i;
      }
        i++;
    }


    str[index + 1] = '\0';
}

void get_RAM(void){

   float total,
         free_mem,
         used;


   char *line = malloc(BUF_SIZE * 3);

   FILE *RAM = fopen("/proc/meminfo", "rt");

   
   // Get total amount of RAM
   fgets(line, BUF_SIZE * 3, RAM);
   truncate_spaces(line);
   sscanf(line, " MemTotal: %f", &total);

   // throw away second line of file
   fgets(line, BUF_SIZE * 3, RAM);

   // Get free memory
   fgets(line, BUF_SIZE * 3, RAM);
   truncate_spaces(line);
   sscanf(line, " MemAvailable: %f", &free_mem);

   fclose(RAM);


   used = total - free_mem;


   printf("%.2f / %.2f GB", used / 1000000, total / 1000000);

   free(line);
}



int main(void){

    get_RAM();

    return 0;
}
