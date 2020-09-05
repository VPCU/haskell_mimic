using System;
using System.IO;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Diagnostics;
using System.Collections;
using System.Text.RegularExpressions;

namespace WindowsFormsApp1
{
    
    public partial class Form1 : Form
    {

        public Form1()
        {
            InitializeComponent();
        }

        private void splitContainer1_Panel2_Paint(object sender, PaintEventArgs e)
        {

        }

        private void Form1_Load(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 4;
        }

        private void KeywordsInitialize()
        {
            StaticVar.keywords.Add("using", "1");
            StaticVar.keywords.Add("public", "1");
            StaticVar.keywords.Add("private", "1");
            StaticVar.keywords.Add("namespace", "1");
            StaticVar.keywords.Add("class", "1");
            StaticVar.keywords.Add("set", "1");
            StaticVar.keywords.Add("get", "1");
            StaticVar.keywords.Add("void", "1");
            StaticVar.keywords.Add("int", "1");
            StaticVar.keywords.Add("string", "1");
            StaticVar.keywords.Add("float", "1");
            StaticVar.keywords.Add("double", "1");
            StaticVar.keywords.Add("for", "1");
            StaticVar.keywords.Add("foreach", "1");
            StaticVar.keywords.Add("in", "1");
            StaticVar.keywords.Add("object", "1");
            StaticVar.keywords.Add("if", "1");
            StaticVar.keywords.Add("else", "1");
            StaticVar.keywords.Add("while", "1");
            StaticVar.keywords.Add("do", "1");
            StaticVar.keywords.Add("partial", "1");
            StaticVar.keywords.Add("switch", "1");
            StaticVar.keywords.Add("case", "1");
            StaticVar.keywords.Add("default", "1");
            StaticVar.keywords.Add("continue", "1");
            StaticVar.keywords.Add("break", "1");
            StaticVar.keywords.Add("return", "1");
            StaticVar.keywords.Add("new", "1");
            StaticVar.keywords.Add("bool", "1");
            StaticVar.keywords.Add("null", "1");
            StaticVar.keywords.Add("false", "1");
            StaticVar.keywords.Add("true", "1");
            StaticVar.keywords.Add("catch", "1");
            StaticVar.keywords.Add("finally", "1");
            StaticVar.keywords.Add("try", "1");
            StaticVar.keywords.Add("enum", "1");
            StaticVar.keywords.Add("extern", "1");
            StaticVar.keywords.Add("inline", "1");
            StaticVar.keywords.Add("char", "1");
            StaticVar.keywords.Add("byte", "1");
            StaticVar.keywords.Add("const", "1");
            StaticVar.keywords.Add("long", "1");
            StaticVar.keywords.Add("protected", "1");
            StaticVar.keywords.Add("short", "1");
            StaticVar.keywords.Add("signed", "1");
            StaticVar.keywords.Add("unsigned", "1");
            StaticVar.keywords.Add("struct", "1");
            StaticVar.keywords.Add("static", "1");
            StaticVar.keywords.Add("this", "1");
            StaticVar.keywords.Add("throw", "1");
            StaticVar.keywords.Add("union", "1");
            StaticVar.keywords.Add("virtual", "1");
            StaticVar.keywords.Add("abstract", "1");
            StaticVar.keywords.Add("event", "1");
            StaticVar.keywords.Add("as", "1");
            StaticVar.keywords.Add("explicit", "1");
            StaticVar.keywords.Add("base", "1");
            StaticVar.keywords.Add("operator", "1");
            StaticVar.keywords.Add("out", "1");
            StaticVar.keywords.Add("params", "1");
            StaticVar.keywords.Add("typeof", "1");
            StaticVar.keywords.Add("uint", "1");
            StaticVar.keywords.Add("ulong", "1");
            StaticVar.keywords.Add("checked", "1");
            StaticVar.keywords.Add("goto", "1");
            StaticVar.keywords.Add("unchecked", "1");
            StaticVar.keywords.Add("readonly", "1");
            StaticVar.keywords.Add("unsafe", "1");
            StaticVar.keywords.Add("implicit", "1");
            StaticVar.keywords.Add("ref", "1");
            StaticVar.keywords.Add("ushort", "1");
            StaticVar.keywords.Add("decimal", "1");
            StaticVar.keywords.Add("sbyte", "1");
            StaticVar.keywords.Add("interface", "1");
            StaticVar.keywords.Add("sealed", "1");
            StaticVar.keywords.Add("volatile", "1");
            StaticVar.keywords.Add("delegate", "1");
            StaticVar.keywords.Add("internal", "1");
            StaticVar.keywords.Add("is", "1");
            StaticVar.keywords.Add("sizeof", "1");
            StaticVar.keywords.Add("lock", "1");
            StaticVar.keywords.Add("stackalloc", "1");
            StaticVar.keywords.Add("var", "1");
            StaticVar.keywords.Add("value", "1");
            StaticVar.keywords.Add("yield", "1");
        }

        /// <summary>
        /// C#语法高亮着色器
        /// </summary>
        /// <param name="start">起始行号</param>
        private void RichHighlight(int start)
        {
            string[] ln = richTextBox1.Text.Split('\n');
            int pos = 0;
            int lnum = 0;
            foreach (string lv in ln)
            {
                if (lnum >= start)
                {
                    string ts = lv.Replace("(", " ").Replace(")", " ");
                    ts = ts.Replace("[", " ").Replace("]", " ");
                    ts = ts.Replace("{", " ").Replace("}", " ");
                    ts = ts.Replace(".", " ").Replace("=", " ").Replace(";", " ");

                    if (lv.Trim().StartsWith("//"))
                    {
                        richTextBox1.Select(pos, lv.Length);
                        richTextBox1.SelectionFont = new Font("微软雅黑", 10.8f, (FontStyle.Regular));
                        richTextBox1.SelectionColor = Color.Green;
                        pos += lv.Length + 1;
                        continue;
                    }
                    if (lv.Trim().StartsWith("#"))
                    {
                        richTextBox1.Select(pos, lv.Length);
                        richTextBox1.SelectionFont = new Font("微软雅黑", 10.8f, (FontStyle.Regular));
                        richTextBox1.SelectionColor = Color.BlueViolet;
                        pos += lv.Length + 1;
                        continue;
                    }

                    ArrayList marks = new ArrayList();
                    string smark = "";
                    string last = "";
                    bool inmark = false;
                    for (int i = 0; i < ts.Length; i++)
                    {
                        if (ts.Substring(i, 1) == "\"" && last != "\\")
                        {
                            if (inmark)
                            {
                                marks.Add(smark + "," + i);
                                smark = "";
                                inmark = false;
                            }
                            else
                            {
                                smark += i;
                                inmark = true;
                            }
                        }
                        last = ts.Substring(i, 1);
                    }
                    if (inmark)
                    {
                        marks.Add(smark + "," + ts.Length);
                    }

                    string[] ta = ts.Split(' ');
                    int x = 0;
                    foreach (string tv in ta)
                    {
                        if (tv.Length < 2)
                        {
                            x += tv.Length + 1;
                            continue;
                        }
                        else
                        {
                            bool find = false;
                            foreach (string px in marks)
                            {
                                string[] pa = px.Split(',');
                                if (x >= Int32.Parse(pa[0]) && x < Int32.Parse(pa[1]))
                                {
                                    find = true;
                                    break;
                                }
                            }
                            if (!find)
                            {
                                if (StaticVar.keywords[tv] != null)
                                {
                                    richTextBox1.Select(pos + x, tv.Length);
                                    richTextBox1.SelectionFont = new Font("微软雅黑", 10.8f, (FontStyle.Regular));
                                    richTextBox1.SelectionColor = Color.Blue;
                                }
                            }
                            x += tv.Length + 1;
                        }
                    }

                    foreach (string px in marks)
                    {
                        string[] pa = px.Split(',');
                        richTextBox1.Select(pos + Int32.Parse(pa[0]), Int32.Parse(pa[1]) - Int32.Parse(pa[0]) + 1);
                        richTextBox1.SelectionFont = new Font("微软雅黑", 10.8f, (FontStyle.Regular));
                        richTextBox1.SelectionColor = Color.DarkOrange;
                    }
                }
                pos += lv.Length + 1;
                lnum++;
            }

            // 设置一下，才能恢复；后续正确！
            richTextBox1.Select(0, 0);
            richTextBox1.SelectionFont = new Font("微软雅黑", 10.8f, (FontStyle.Regular));
            richTextBox1.SelectionColor = Color.Black;
            richTextBox1.SelectionStart = StaticVar.cursor;
            richTextBox1.Focus();
        }

        private void Form1_KeyDown(object sender, KeyEventArgs e)
        {

        }
        private void 打开ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            OpenFileDialog dialog = new OpenFileDialog();
            dialog.Multiselect = false;//该值确定是否可以选择多个文件
            dialog.Title = "请选择源程序文件";
            dialog.Filter = "MiniC源程序(*.c)|*.c";
            if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                StaticVar.infile = dialog.FileName;
            }
            this.保存ToolStripMenuItem.Enabled = true;
            this.另存为ToolStripMenuItem.Enabled = true;
            this.toolStripButton2.Enabled = true;
            this.另存为ToolStripMenuItem.Enabled = true;
            string text = System.IO.File.ReadAllText(StaticVar.infile);
            this.richTextBox1.Text = text;
            //Console.WriteLine(dialog.FileName);
        }

        private void richTextBox1_TextChanged(object sender, EventArgs e)
        {
            this.toolStripButton2.Enabled = true;
            this.编译ToolStripMenuItem.Enabled = true;
            StaticVar.cursor = richTextBox1.SelectionStart;
            this.RichHighlight(0);
            //this.RichHighlight(this.richTextBox1.Lines.Length-1);
        }

        private void 运行ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 4;
            保存ToolStripMenuItem_Click(sender, e);
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string text = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, text);
            }

            RunCmd("micc.exe \"" + StaticVar.infile + "\"");
            RunCmd("gcc.exe " + "\"" + StaticVar.OutFile() + "\" -o \"" + StaticVar.ExeFile() + "\"");
            RunCmd2($"/C \"" + StaticVar.ExeFile()+"\" & pause");
            //string result = RunCmd2(StaticVar.infile + " > 1.sgcc 1.s - o 11.exe");
            //System.IO.File.WriteAllText(StaticVar.Resultfile(), result);
            //string text = System.IO.File.ReadAllText(StaticVar.Resultfile());
            //this.richTextBox2.Text = text;
        }

        private void richTextBox2_TextChanged(object sender, EventArgs e)
        {
            
        }

        public static string RunCmd(string cmd)
        {
            //string strInput = Console.ReadLine();
            Process p = new Process();
            //设置要启动的应用程序
            p.StartInfo.FileName = "cmd.exe";
            //是否使用操作系统shell启动
            p.StartInfo.UseShellExecute = false;
            // 接受来自调用程序的输入信息
            p.StartInfo.RedirectStandardInput = true;
            //输出信息
            p.StartInfo.RedirectStandardOutput = true;
            // 输出错误
            p.StartInfo.RedirectStandardError = true;
            //不显示程序窗口
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.WindowStyle = ProcessWindowStyle.Normal;
            //启动程序
            p.Start();

            //向cmd窗口发送输入信息
            p.StandardInput.WriteLine(cmd + "&exit");

            p.StandardInput.AutoFlush = true;

            //获取输出信息
            string strOuput = p.StandardOutput.ReadToEnd();
            //等待程序执行完退出进程
            p.WaitForExit();
            p.Close();
            return strOuput;
            //Console.WriteLine(strOuput);
        }

        public static void RunCmd2(string cmd)
        {
            ProcessStartInfo p = new ProcessStartInfo();
            p.FileName = "cmd.exe";
            p.Arguments = cmd;
            p.UseShellExecute = true;
            p.WindowStyle = ProcessWindowStyle.Normal;
            Process.Start(p);
        }

        private void 生成汇编ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 3;
            保存ToolStripMenuItem_Click(sender, e);
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string t = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, t);
            }

            RunCmd("micc.exe \"" + StaticVar.infile + "\"");
            string text = System.IO.File.ReadAllText(StaticVar.OutFile());
            this.richTextBox2.Text = text;
        }

        private void 保存ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string t = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, t);
            }
            string text = this.richTextBox1.Text;
            System.IO.File.WriteAllText(StaticVar.infile, text);
            text = System.IO.File.ReadAllText(StaticVar.infile);
            this.richTextBox1.Text = text;
        }

        private void 另存为ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            SaveFileDialog dialog = new SaveFileDialog();
            dialog.Title = "另存为";
            dialog.Filter = "MiniC源程序(*.c)|*.c";
            dialog.InitialDirectory = "d:\\";
            dialog.FilterIndex = 2;
            dialog.RestoreDirectory = true;
            DialogResult dr = dialog.ShowDialog();
            if (dr == DialogResult.OK && dialog.FileName.Length > 0)
            {
                richTextBox1.SaveFile(dialog.FileName, RichTextBoxStreamType.PlainText);
                MessageBox.Show("另存成功！", "保存文件");
            }
        }

        private void splitContainer1_SplitterMoved(object sender, SplitterEventArgs e)
        {

        }

        private void toolStripButton4_Click(object sender, EventArgs e)
        {
            新建ToolStripMenuItem_Click(sender, e);
        }

        private void 新建ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            SaveFileDialog dialog = new SaveFileDialog();
            dialog.Title = "新建";
            dialog.Filter = "MiniC源程序(*.c)|*.c";
            dialog.InitialDirectory = "d:\\";
            dialog.FilterIndex = 2;
            dialog.RestoreDirectory = true;
            DialogResult dr = dialog.ShowDialog();
            if (dr == DialogResult.OK && dialog.FileName.Length > 0)
            {
                richTextBox1.SaveFile(dialog.FileName, RichTextBoxStreamType.PlainText);
                //MessageBox.Show("创建成功！", "新建");
            }
            this.保存ToolStripMenuItem.Enabled = true;
            this.另存为ToolStripMenuItem.Enabled = true;
            this.toolStripButton2.Enabled = true;
        }

        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            打开ToolStripMenuItem_Click(sender, e);
        }

        private void toolStripButton2_Click(object sender, EventArgs e)
        {
            保存ToolStripMenuItem_Click(sender, e);
        }

        private void toolStripButton3_Click(object sender, EventArgs e)
        {
            if(this.toolStripComboBox1.SelectedIndex == 3)
            {
                生成汇编ToolStripMenuItem_Click(sender, e);
            }
            else if(this.toolStripComboBox1.SelectedIndex == 2)
            {
                中间代码ToolStripMenuItem_Click(sender, e);
            }
            else if (this.toolStripComboBox1.SelectedIndex == 1)
            {
                词法分析ToolStripMenuItem_Click(sender, e);
            }
            else if (this.toolStripComboBox1.SelectedIndex == 0)
            {
                语法树ToolStripMenuItem_Click(sender, e);
            }
            else
            {
                运行ToolStripMenuItem_Click(sender, e);
            }
        }

        private void 词法分析ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 1;
            保存ToolStripMenuItem_Click(sender, e);
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string t = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, t);
            }

            RunCmd("micc.exe \"" + StaticVar.infile + "\"");
            string text = System.IO.File.ReadAllText(StaticVar.ParseFile());
            this.richTextBox2.Text = text;
        }

        private void 中间代码ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 1;
            保存ToolStripMenuItem_Click(sender, e);
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string t = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, t);
            }

            RunCmd("micc.exe \"" + StaticVar.infile + "\"");
            string text = System.IO.File.ReadAllText(StaticVar.TemFile());
            this.richTextBox2.Text = text;
        }

        private void toolStripComboBox1_Click(object sender, EventArgs e)
        {

        }

        private void 按选项运行ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            toolStripButton3_Click(sender, e); //执行单击button3的动作
        }

        private void 语法树ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 0;
            保存ToolStripMenuItem_Click(sender, e);
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string t = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, t);
            }

            RunCmd("micc.exe \"" + StaticVar.infile + "\"");
            string text = System.IO.File.ReadAllText(StaticVar.ParseFile());
            this.richTextBox2.Text = text;
        }

        private void 关于ToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void 帮助ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string text = System.IO.File.ReadAllText("bangzhu.txt");
            this.richTextBox2.Text = text;
        }

        private void 代码优化ToolStripMenuItem_Click(object sender, EventArgs e)
        {
            this.toolStripComboBox1.SelectedIndex = 2;
            保存ToolStripMenuItem_Click(sender, e);
            if (StaticVar.infile == null)
            {
                StaticVar.infile = "NewMiniC.c";
                string t = richTextBox1.Text;
                System.IO.File.WriteAllText(StaticVar.infile, t);
            }

            RunCmd("micc.exe \"" + StaticVar.infile + "\"");
            string text = System.IO.File.ReadAllText(StaticVar.ProFile());
            this.richTextBox2.Text = text;
        }
    }
    class StaticVar
    {
        public static string infile = null;
        public static int cursor;
        public static Hashtable keywords = new Hashtable();
        public static string OutFile () { return infile + ".s"; }
        public static string TemFile () { return infile + ".tem"; }
        public static string ProFile () { return infile + ".pro"; }
        public static string ExeFile () { return infile + ".exe"; }
        public static string ParseFile() { return infile + ".parse"; }
    }
}
