/^[A-Z]:/ {
      command = "tr \\\\\\\\\ / | xargs cygpath "; 
      printf "%s", $1 | command;  
      close(command); 
      for (i = 2; i < NF; i++) {
        printf " %s", $i;
      };
      printf "\n";

      next;
}
{print}