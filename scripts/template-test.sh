#!/usr/bin/env bash


# [templates]
# log_node='''
# coalesce(
#   if(!self, label("elided", "~")),
#   label(
#     separate(" ",
#       if(current_working_copy, "working_copy"),
#       if(immutable, "immutable"),
#       if(conflict, "conflict"),
#     ),
#     coalesce(
#       if(current_working_copy, "@"),
#       if(immutable, "◆"),
#       if(conflict, "×"),
#       if(self.contained_in("private()"), "◌"),
#       "○",
#     )
#   )
# )'''

# This is the start. now we add metadata before and above
# "$$--"++if(!self, "YES")++"--$$\n"++
# jj log -T '
# "$$--START--$$"++"|"++change_id++"|"++commit_id++"|\n"++
# "$$--END--$$\n"
# '
TMP='
if(root,
  format_root_commit(self),
  label(if(current_working_copy, "working_copy"),
    concat(
      format_short_commit_header(self)++"\n" ,
      separate(" ",
        if(empty, label("empty", "(empty)")),
        if(description,
          description.first_line(),
          label(if(empty, "empty"), description_placeholder),
        ),
      ) ,
    ),
  )
)'

IDS='"|"++++"|"++commit_id++"|"'
TMP="$IDS++$TMP"
jj log -T "
\"\$\$--START--\$\$\"++$TMP++\"\$\$--END--\$\$\"
"
