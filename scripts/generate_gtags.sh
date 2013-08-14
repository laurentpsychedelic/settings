#!/bin/bash
find . -type f -regex '.*\(java$\|groovy\$\|h$\|hpp$\|cpp$\|HPP$\|hpp$\)' -print0 | xargs -0 etags
