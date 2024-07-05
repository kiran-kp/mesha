(uiop:chdir "/home/kiran/projects/mesha")
(ql:quickload "cffi")
(pushnew #P"/home/kiran/projects/mesha/build/" cffi:*foreign-library-directories*)
(pushnew #P"/home/kiran/projects/mesha/extern/" asdf:*central-registry*)
(ql:quickload "mesha")

