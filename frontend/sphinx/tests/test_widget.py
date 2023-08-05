import unittest

from widget.widget import Widget, NameException, ButtonException, ChopException, LabException, FileException, MainException

class TestWidget(unittest.TestCase):
    def setUp(self):
        self.widget = Widget()

    def test_parseArgs_name(self):
        name = "test"
        self.widget.parseArgs(["project=" + name])
        self.assertEqual(self.widget.name, name)
        self.assertFalse(self.widget.is_lab)

    def test_parseArgs_lab(self):
        name = "test"
        self.widget.parseArgs(["lab=" + name])
        self.assertEqual(self.widget.name, name)
        self.assertTrue(self.widget.is_lab)
        self.assertEqual(len(self.widget.button_group), 1)
        self.assertEqual(self.widget.button_group[0].mode, "submit")

    def test_parseArgs_noname(self):
        with self.assertRaises(NameException):
            self.widget.name

    def test_parseArgs_button(self):
        self.widget.parseArgs(["run_button"])
        self.assertEqual(len(self.widget.button_group), 1)
        self.assertEqual(self.widget.button_group[0].mode, "run")

    def test_parseArgs_nobutton(self):
        self.widget.parseArgs(["no_button", "run_button"])
        self.assertEqual(len(self.widget.button_group), 0)

    def test_parseArgs_missingbutton(self):
       with self.assertRaises(ButtonException):
           self.widget.button_group

    def test_parseArgs_switches(self):
        switches_in = "A(a,b,c);B(d,e,f);A(g)"
        switches_out = {
            "Builder": [],
            "Compiler": ["-gnata"],
            "A": ["a", "b", "c", "g"],
            "B": ["d", "e", "f"]
        }
        self.widget.parseArgs(["switches=" + switches_in])
        self.assertDictEqual(self.widget.switches, switches_out)

    def test_parseArgs_nomain(self):
        self.assertEqual(self.widget.main, "")

    def test_parseArgs_main(self):
        ada = """package body A is

end A;

package A is

end A;

procedure Test is
begin
   null;
end Test;
"""
        self.widget.parseArgs(["main=test.adb", "ada"])
        self.widget.parseContent(ada.splitlines())
        self.assertEqual(self.widget.main, "test.adb")

    def test_parseArgs_wrongmain(self):
        ada = """package body A is

end A;

package A is

end A;

procedure Test is
begin
   null;
end Test;
"""
        self.widget.parseArgs(["main=wrong.adb", "ada"])
        self.widget.parseContent(ada.splitlines())
        with self.assertRaises(MainException):
            self.widget.main

    def test_parseArgs_invalid(self):
        with self.assertRaises(ValueError):
            self.widget.parseArgs(['invalid'])

    def test_parseOpts_nocheck(self):
        self.widget.parseArgs(["run_button"])
        self.assertEqual(len(self.widget.button_group), 1)
        self.widget.parseOpts({"class": ["ada-nocheck"]})
        self.assertEqual(len(self.widget.button_group), 0)

    def test_parseOpts_syntaxonly(self):
        self.widget.parseArgs(["run_button"])
        self.assertEqual(len(self.widget.button_group), 1)
        self.widget.parseOpts({"class": ["ada-syntax-only"]})
        self.assertEqual(len(self.widget.button_group), 0)

    def test_parseOpts_invalid(self):
        with self.assertRaises(ValueError):
            self.widget.parseOpts({"class": ["invalid"]})

    def test_nofiles(self):
        with self.assertRaises(FileException):
            self.widget.files

    def test_parseContent_adasimple(self):
        ada = """package body A is

end A;

package A is

end A;

procedure Main is
begin
   null;
end Main;
"""
        self.widget.parseArgs(["project=Test", "ada"])
        self.widget.parseContent(ada.splitlines())
        self.assertEqual(len(self.widget.files), 3)

    def test_parseContent_csimple(self):
        c = """Garbage
More garbage
!fake_file.txt
blahblah


nothing

!test.c
test.c

test.c

!test.h
test.h

test.h"""
        self.widget.parseArgs(["project=Test", "c"])
        self.widget.parseContent(c.splitlines())
        self.assertEqual(len(self.widget.files), 2)

    def test_parseContent_lab(self):
        lab = """
--  START LAB IO BLOCK
lab_io
--  END LAB IO BLOCK

package body A is

end A;

package A is

end A;

procedure Main is
begin
   null;
end Main;
"""
        split = lab.splitlines()
        self.widget.parseArgs(["ada", "lab=Test"])
        self.widget.parseContent(split)
        self.assertEqual(len(self.widget.shadow_files), 1)
        self.assertEqual(self.widget.shadow_files[0].basename, "lab_io.txt")
        self.assertEqual(self.widget.shadow_files[0].content, split[2])

        self.assertEqual(len(self.widget.files), 3)

    def test_parseContent_lab_dup_startblock(self):
        lab = """

--  START LAB IO BLOCK
lab_io
--  START LAB IO BLOCK
--  END LAB IO BLOCK

"""
        self.widget.parseArgs(["manual_chop", "lab=Test"])
        with self.assertRaises(LabException):
            self.widget.parseContent(lab.splitlines())

    def test_parseContent_lab_endfirst(self):
        lab = """
--  END LAB IO BLOCK
--  START LAB IO BLOCK
"""
        self.widget.parseArgs(["manual_chop", "lab=Test"])
        with self.assertRaises(LabException):
            self.widget.parseContent(lab.splitlines())

    def test_parseContent_lab_no_end(self):
        lab = """
--  START LAB IO BLOCK
"""
        self.widget.parseArgs(["manual_chop", "lab=Test"])
        with self.assertRaises(LabException):
            self.widget.parseContent(lab.splitlines())

    def test_parseContent_nochop(self):
        self.widget.parseArgs(["project=Test"])
        with self.assertRaises(ChopException):
            self.widget.parseContent([])


if __name__ == '__main__':
    unittest.main()
