# CMake script to patch ifs.rsp file

file(STRINGS "${filename}" content NEWLINE_CONSUME)
string(REPLACE "-Wl,-rpath," "-rpath " content "${content}")
string(REPLACE "-Wl,-u," "-u " content "${content}")
string(REPLACE "-Wl,-U," "-U " content "${content}")
string(REPLACE "-Wl,-rpath-link," "-rpath-link " content "${content}")
file(WRITE "${filename}" "${content}")
