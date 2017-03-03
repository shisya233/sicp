#!/usr/bin/python
#coding:utf-8
#Author:ldx228@tju.edu.cn
import os,sys

def load():
    if len(sys.argv) == 2:
        scm = sys.argv[1]
        f = open("doctest_tmp","w")
        f.write(scm)
        f.close()
    else:
        scm = open("doctest_tmp","r").read()
    scripts = open(scm,"r").readlines()
    for i in range(len(scripts)):
        if scripts[i] == ";test\n":
            a = scripts[:i]
            b = scripts[i+1:]
            return (a,b)
    b = '(display "do not have test code")\n'
    return (scripts,b)
def edit(test_code):
    a,b = test_code
    def edit_line(tcode,answer):
        if answer == "'print":
            edited_code = '(display (cons "print %s -->" %s))\n' % (tcode,tcode)
        else:
            edited_code = '(if (equal? %s %s) (display "(test succeed %s .%s)") (display (cons "test failed  %s .%s" %s)))\n'\
                    % (tcode,answer,tcode,answer,tcode,answer,tcode)
        return edited_code
    tmp = []
    for line in b:
        if line == "\n":
            continue
        elif line[1] == ";":
            tmp.append(edit_line(tmp.pop()[:-1],line[2:-1]))
            tmp.append("(newline)\n")
        else:
            tmp.append(line[1:])
    a.extend(tmp)
    return a

def test(code):
    fname = "doctest.scm"
    f = open(fname, "w")
    f.writelines(code)
    f.close()
    output = os.popen("csi -s " + fname)
    print output.read()
    os.system("rm "+fname)

def main():
    test_code = load()
    test_code = edit(test_code)
    test(test_code)
    return 0

if __name__ == "__main__":
    main()
