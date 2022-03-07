#include <iostream>
#include <cmath>
#include <string>
#include <fstream>
#include <vector>
#include <algorithm>
#include <bits/stdc++.h>
using namespace std;

struct Labels
{
    string label;
    int32_t addr;
};

struct memory
{
    string label;
    int32_t val;
};

class Simulator
{
private:
    string Registers[32];
    int32_t RegValues[32];
    string Instnset[17];
    vector<string> Program;
    int32_t No_ofInstructions;
    string curr_Inst;
    int32_t prgmcountr;
    int32_t end_prgm;
    int32_t maxlength;
    int32_t mode;
    int32_t reg[3];
    vector<struct Labels> table;
    vector<struct memory> mem;
    int32_t stack[100];
    void add();
    void addi();
    void sub();
    void mul();
    void andf();
    void orf();
    void andi();
    void ori();
    void nor();
    void slt();
    void slti();
    void lw();
    void sw();
    void beq();
    void bne();
    void j();
    void end();
    void preprocess();
    void readin(int32_t line);
    int32_t parsein();
    void error();
    void executein(int32_t inst);
    void onlyspaces(int32_t l, int32_t u, string s);
    void findreg(int32_t num);
    void removespaces(string &str);
    string findlabel();
    void assnum(string s);
    void stack_bounds(int32_t in);
    void assremcomma();
    void asslaball(string s);

public:
    Simulator(int32_t m, string file);
    void execute();
    void display();
};
int32_t sortreg(Labels a, Labels b);
int32_t sortmem(memory a, memory b);
void Simulator::execute()
{
    getchar();
    preprocess();
    while (prgmcountr < No_ofInstructions && end_prgm == 0)
    {
        readin(prgmcountr);
        removespaces(curr_Inst);
        if (curr_Inst == "")
        {
            prgmcountr++;
            continue;
        }
        int32_t inst = parsein();
        executein(inst);
        if (inst < 13 || inst > 15)
        {
            prgmcountr++;
        }
        if (mode == 0 && end_prgm == 0)
        {
            display();
            getchar();
        }
        display();
        if (end_prgm == 0)
        {
            cout << "Error : There is no end statement" << endl;
            exit(1);
        }
        cout << endl;
        cout << "Program ran successfully" << endl;
    }
}
Simulator::Simulator(int32_t m, string file)
{
    maxlength = 10000;
    No_ofInstructions = 0;
    prgmcountr = 0;
    end_prgm = 0;
    mem.clear();
    table.clear();
    string registers[] = {"r0", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
                          "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "t8", "t9", "k0", "k1", "gp", "sp", "s8", "ra"};
    for (int32_t i = 0; i < 32; i++)
    {
        Registers[i] = registers[i];
    }
    for (int32_t i = 0; i < 32; i++)
    {
        RegValues[i] = 0;
    }
    string instnset[] = {
        "add",
        "sub",
        "mul",
        "and",
        "or",
        "nor"
        "slt",
        "addi",
        "andi",
        "ori",
        "slti",
        "lw",
        "sw",
        "beq",
        "bne",
        "j",
        "end",
    };
    for (int32_t i = 0; i < 17; i++)
    {
        Instnset[i] = instnset[i];
    }
    for (int32_t i = 0; i < 100; i++)
    {
        stack[i] = 0;
    }
    RegValues[28] = 100000000;
    RegValues[29] = 40396;
    mode = m;
    ifstream input;
    input.open(file.c_str(), ios::in);
    if (!input)
    {
        cout << "Error : This file cannot be opened or doesn't exist";
        exit(1);
    }
    string curr_string;
    while (getline(input, curr_string))
    {
        No_ofInstructions++;
        if (No_ofInstructions > maxlength)
        {
            cout << "Error : Lines exceeded max length";
            exit(1);
        }
        Program.push_back(curr_string);
    }
    input.close();
}

void Simulator::preprocess()
{
    int32_t i = 0, j = 0;
    int32_t curr_sec = -1;
    int32_t index;
    int32_t flag = 0;
    string curr_string = "";
    int islabel;
    int Flag = 0;
    int32_t sectionstart = 0;
    int32_t textstart = 0;
    for (i = 0; i < No_ofInstructions; i++)
    {
        readin(i);
        if (curr_Inst == "")
        {
            continue;
        }
        index = curr_Inst.find(".data");
        if (index == -1)
        {
            continue;
        }
        else if (flag == 0)
        {
            flag = 1;
            onlyspaces(0, index, curr_Inst);
            onlyspaces(index + 5, curr_Inst.size(), curr_Inst);
            curr_sec = 0;
            sectionstart = i;
        }
        else if (flag == 1)
        {
            cout << "Error : multiple instances of .data" << endl;
            error();
        }
    }
    int32_t lablin;
    if (curr_sec == 0)
    {
        for (i = sectionstart + 1; i < No_ofInstructions; i++)
        {
            readin(i);
            removespaces(curr_Inst);
            if (curr_Inst == "")
            {
                continue;
            }
            lablin = curr_Inst.find(":");
            if (lablin == -1)
            {
                if (curr_Inst.find(".text") == -1)
                {
                    cout << "Error : unexpected line in .data" << endl;
                    error();
                }
                else
                {
                    break;
                }
            }
            if (lablin == 0)
            {
                cout << "Error : label name must be present" << endl;
                error();
            }
            j = lablin - 1;
            while (j >= 0 && curr_Inst[j] == ' ' || curr_Inst[j] == '\t')
            {
                j--;
            }
            curr_string = "";
            int32_t Flag = 0;
            for (; j >= 0; j--)
            {
                if (curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && Flag == 0)
                {
                    curr_string = curr_Inst[j] + curr_string;
                }
                else if (curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && Flag == 1)
                {
                    cout << "Error : Unexpected line before label name" << endl;
                    error();
                }
                else
                {
                    Flag = 1;
                }
            }
            asslaball(curr_string);
            memory curr_mem;
            curr_mem.label = curr_string;
            int32_t wordin = curr_Inst.find(".word");
            onlyspaces(lablin + 1, wordin, curr_Inst);
            int32_t find_value = 0;
            int32_t finished_finding = 0;
            curr_string = "";
            for (j = wordin + 5; j < curr_Inst.size(); j++)
            {
                if (find_value == 1 && (curr_Inst[j] == ' ' || curr_Inst[j] == '\t') && finished_finding == 0)
                {
                    finished_finding = 1;
                }
                else if (find_value == 1 && curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && finished_finding == 1)
                {
                    cout << "Error : Unexpected line after the value" << endl;
                    error();
                }
                else if (find_value == 0 && curr_Inst[j] != ' ' && curr_Inst[j] != '\t')
                {
                    find_value = 1;
                    curr_string = curr_string + curr_Inst[j];
                }
                else if (find_value == 1 && curr_Inst[j] != ' ' && curr_Inst[j] != '\t')
                {
                    curr_string = curr_string + curr_Inst[j];
                }
            }
            assnum(curr_string);
            curr_mem.val = stoi(curr_string);
            mem.push_back(curr_mem);
        }
    }
    sort(mem.begin(), mem.end(), sortmem);
    for (int i = 0; mem.size() > 0 && i < mem.size() - 1; i++)
    {
        if (mem[i].label == mem[i + 1].label)
        {
            cout << "Error : label names are repeated" << endl;
            exit(1);
        }
    }
    int32_t textflag = 0;
    int32_t textin = 0;
    for (i = prgmcountr; i < No_ofInstructions; i++)
    {
        readin(i);
        if (curr_Inst == "")
        {
            continue;
        }
        textin = curr_Inst.find(".text");
        if (textin == -1)
        {
            continue;
        }
        else if (textflag == 0)
        {
            textflag = 1;
            onlyspaces(0, textin, curr_Inst);
            onlyspaces(textin + 5, curr_Inst.size(), curr_Inst);
            curr_sec = 1;
            textstart = i;
        }
        else if (textflag == 1)
        {
            cout << "Error : mutiple instances of .text" << endl;
            error();
        }
    }
    if (curr_sec != 1)
    {
        cout << "Error : Text segment not found" << endl;
        exit(1);
    }
    int32_t mainin = 0;
    int32_t findmain = 0;
    lablin = 0;
    for (i = textin + 1; i < No_ofInstructions; i++)
    {
        readin(i);
        if (curr_Inst == "")
        {
            continue;
        }
        lablin = curr_Inst.find(":");
        if (lablin == 0)
        {
            cout << "Error : label name must be there" << endl;
            error();
        }
        if (lablin == -1)
        {
            continue;
        }
        j = lablin - 1;
        while (j >= 0 && curr_Inst[j] == ' ' || curr_Inst[j] == '\t')
        {
            j--;
        }
        curr_string = "";
        islabel = 0;
        Flag = 0;
        for (; j >= 0; j--)
        {
            if (curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && Flag == 0)
            {
                islabel = 1;
                curr_string = curr_Inst[j] + curr_string;
            }
            else if (curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && Flag == 1)
            {
                cout << "Error : Unexpected text before label" << endl;
                error();
            }
            else if (islabel == 0)
            {
                cout << "Error : Label name must be there" << endl;
                error();
            }
            else
            {
                Flag = 1;
            }
        }
        asslaball(curr_string);
        onlyspaces(lablin + 1, curr_Inst.size(), curr_Inst);
        if (curr_string == "main")
        {
            findmain = 1;
            mainin = prgmcountr + 1;
        }
        else
        {
            Labels currlabl;
            currlabl.addr = prgmcountr;
            currlabl.label = curr_string;
            table.push_back(currlabl);
        }
    }
    sort(table.begin(), table.end(), sortreg);
    for (i = 0; table.size() > 0 && i < (table.size() - 1); i++)
    {
        if (table[i].label == table[i + 1].label)
        {
            cout << "Error : labels are repeated" << endl;
            exit(1);
        }
    }
    if (findmain == 0)
    {
        cout << "Error : main is not found";
        exit(1);
    }
    prgmcountr = mainin;
    cout << "Program is initialised and ready to execute" << endl;
    cout << "Current state is " << endl;
    display();
    cout << endl;
    cout << "stating the execution" << endl;
    cout << endl;
}

void Simulator::error()
{
    cout << "Error found in this line : " << (prgmcountr + 1) << ":" << Program[prgmcountr] << endl;
    display();
    exit(1);
}

void Simulator::readin(int32_t line)
{
    curr_Inst = Program[line];
    if (curr_Inst.find("#") != -1)
    {
        curr_Inst = curr_Inst.substr(0, curr_Inst.find("#"));
    }
    prgmcountr = line;
}

int32_t Simulator::parsein()
{
    int32_t i = 0, j = 0;
    removespaces(curr_Inst);
    if (curr_Inst.find(":") != -1)
    {
        return -2;
    }
    if (curr_Inst.size() < 4)
    {
        cout << "Error : operation is unknown" << endl;
    }
    for (j = 0; j < 4; j++)
    {
        if (curr_Inst[j] == ' ' || curr_Inst[j] == '\t')
        {
            break;
        }
    }
    string opertn = curr_Inst.substr(0, j);
    if (curr_Inst.size() > 0 && j < curr_Inst.size() - 1)
    {
        curr_Inst = curr_Inst.substr(j + 1);
    }
    int32_t findop = 0;
    int32_t opid = -1;
    for (i = 0; i < 17; i++)
    {
        if (opertn == Instnset[i])
        {
            opid = i;
            break;
        }
    }
    if (opid == -1)
    {
        cout << "Error : operation is not known" << endl;
        error();
    }
    if (opid < 7)
    {
        for (int32_t count = 0; count < 3; count++)
        {
            removespaces(curr_Inst);
            findreg(count);
            removespaces(curr_Inst);
            if (count == 2)
            {
                break;
            }
            assremcomma();
        }
        if (curr_Inst != "")
        {
            cout << "Error : There are extra arguments" << endl;
            error();
        }
    }
    else if (opid < 11)
    {
        for (int32_t count = 0; count < 2; count++)
        {
            removespaces(curr_Inst);
            findreg(count);
            removespaces(curr_Inst);
            assremcomma();
        }
        removespaces(curr_Inst);
        string curr_string = findlabel();
        assnum(curr_string);
        reg[2] = stoi(curr_string);
    }
    else if (opid < 13)
    {
        string curr_string = "";
        int32_t offset;
        removespaces(curr_Inst);
        findreg(0);
        removespaces(curr_Inst);
        assremcomma();
        removespaces(curr_Inst);
        if ((curr_Inst[0] > 47 && curr_Inst[0] < 58) || curr_Inst[0] == '-')
        {
            j = 0;
            while (j < curr_Inst.size() && curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && curr_Inst[j] != '(')
            {
                curr_string = curr_string + curr_Inst[j];
                j++;
            }
            if (j == curr_Inst.size())
            {
                cout << "Error : '(' expected" << endl;
                error();
            }
            assnum(curr_string);
            offset = stoi(curr_string);
            curr_Inst = curr_Inst.substr(j);
            removespaces(curr_Inst);
            if (curr_Inst == "" || curr_Inst[0] != '(' || curr_Inst.size() < 2)
            {
                cout << "Error : '(' expected" << endl;
                error();
            }
            curr_Inst = curr_Inst.substr(1);
            removespaces(curr_Inst);
            findreg(1);
            removespaces(curr_Inst);
            if (curr_Inst == "" || curr_Inst[0] != ')')
            {
                cout << "Error : ')' expected" << endl;
                error();
            }
            curr_Inst = curr_Inst.substr(1);
            onlyspaces(0, curr_Inst.size(), curr_Inst);
            reg[2] = offset;
            if (reg[2] == -1)
            {
                cout << "Error : Offset is invalid" << endl;
                error();
            }
        }
        else
        {
            curr_string = findlabel();
            int32_t findloc = 0;
            for (j = 0; j < mem.size(); j++)
            {
                if (curr_string == mem[j].label)
                {
                    findloc = 1;
                    if (opid == 11)
                    {
                        reg[1] = mem[j].val;
                    }
                    else
                    {
                        reg[1] = j;
                    }
                    break;
                }
            }
            if (findloc == 0)
            {
                cout << "Error : label is invalid" << endl;
                error();
            }
            reg[2] = -1;
        }
    }
    else if (opid < 15)
    {
        for (int32_t count = 0; count < 2; count++)
        {
            removespaces(curr_Inst);
            findreg(count);
            removespaces(curr_Inst);
            assremcomma();
        }
        removespaces(curr_Inst);
        string curr_string = findlabel();
        int32_t findadd = 0;
        for (j = 0; j < table.size(); j++)
        {
            if (curr_string == table[j].label)
            {
                findadd = 1;
                reg[2] = table[j].addr;
                break;
            }
        }
        if (findadd == 0)
        {
            cout << "Error : label is invalid" << endl;
            error();
        }
    }
    else if (opid == 15)
    {
        removespaces(curr_Inst);
        int32_t findadd = 0;
        string curr_string = findlabel();
        for (j = 0; j < table.size(); j++)
        {
            if (curr_string == table[j].label)
            {
                findadd = 1;
                reg[0] = table[j].addr;
            }
        }
        if (findadd == 0)
        {
            cout << "Error : label is invalid" << endl;
            error();
        }
    }
    else if (opid == 16)
    {
        removespaces(curr_Inst);
    }
    return opid;
}
void Simulator::onlyspaces(int32_t l, int32_t u, string s)
{
    for (int32_t i = l; i < u; i++)
    {
        if (s[i] != ' ' && s[i] != '\t')
        {
            cout << "Error : character is unexpected" << endl;
            error();
        }
    }
}
void Simulator::executein(int32_t inst)
{
    switch (inst)
    {
    case 0:
        add();
        break;
    case 1:
        sub();
        break;
    case 2:
        mul();
        break;
    case 3:
        andf();
        break;
    case 4:
        orf();
        break;
    case 5:
        nor();
        break;
    case 6:
        slt();
        break;
    case 7:
        addi();
        break;
    case 8:
        andi();
        break;
    case 9:
        ori();
        break;
    case 10:
        slti();
        break;
    case 11:
        lw();
        break;
    case 12:
        sw();
        break;
    case 13:
        beq();
        break;
    case 14:
        bne();
        break;
    case 15:
        j();
        break;
    case 16:
        end();
        break;
    case -2:
        break;
    default:
        cout << "Error : invalid instruction" << endl;
        error();
    }
}
void Simulator::removespaces(string &s)
{
    int32_t j = 0;
    while (j < s.size() && (s[j] == ' ' || s[j] == '\t'))
    {
        j++;
    }
    s = s.substr(j);
}
void Simulator::add()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] + RegValues[reg[2]]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] + RegValues[reg[2]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}
void Simulator::addi()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] + reg[2]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] + reg[2];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}
void Simulator::sub()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] - RegValues[reg[2]]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] - RegValues[reg[2]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}
void Simulator::mul()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] * RegValues[reg[2]]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] * RegValues[reg[2]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}
void Simulator::andf()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] & RegValues[reg[2]]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] & RegValues[reg[2]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}
void Simulator::andi()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] & reg[2]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] & reg[2];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::orf()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] | RegValues[reg[2]]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] | RegValues[reg[2]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::ori()
{
    if (reg[0] == 29)
    {
        stack_bounds(RegValues[reg[1]] | reg[2]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] | reg[2];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::nor()
{
    if (reg[0] == 29)
    {
        stack_bounds(~(RegValues[reg[1]] | RegValues[reg[2]]));
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = ~(RegValues[reg[1]] | RegValues[reg[2]]);
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::slt()
{
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1 && reg[2] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] < RegValues[reg[2]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::slti()
{
    if (reg[0] != 0 && reg[0] != 1 && reg[1] != 1)
    {
        RegValues[reg[0]] = RegValues[reg[1]] < reg[2];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::lw()
{
    if (reg[0] == 29)
    {
        stack_bounds(reg[1]);
    }
    if (reg[0] != 0 && reg[0] != 1 && reg[2] == -1)
    {
        RegValues[reg[0]] = reg[1];
    }
    else if (reg[0] != 0 && reg[0] != 1)
    {
        stack_bounds(RegValues[reg[1]] + reg[2]);
        RegValues[reg[0]] = stack[(RegValues[reg[1]] + reg[2] - 40000) / 4];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::sw()
{
    if (reg[0] != 1 && reg[2] == -1)
    {
        mem[reg[1]].val = RegValues[reg[0]];
    }
    else if (reg[0] != 1)
    {
        stack_bounds(RegValues[reg[1]] + reg[2]);
        stack[(RegValues[reg[1]] + reg[2] - 40000) / 4] = RegValues[reg[0]];
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::beq()
{
    if (reg[0] != 1 && reg[1] != 1)
    {
        if (RegValues[reg[0]] == RegValues[reg[1]])
        {
            prgmcountr = reg[2];
        }
        else
        {
            prgmcountr++;
        }
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::bne()
{
    if (reg[0] != 1 && reg[1] != 1)
    {
        if (RegValues[reg[0]] != RegValues[reg[1]])
        {
            prgmcountr = reg[2];
        }
        else
        {
            prgmcountr++;
        }
    }
    else
    {
        cout << "Error: Invalid usage of registers" << endl;
        error();
    }
}

void Simulator::j()
{
    prgmcountr = reg[0];
}

void Simulator::end()
{
    end_prgm = 1;
}

void Simulator::display()
{
    int32_t curr_add = 40000;
    if (prgmcountr < No_ofInstructions)
    {
        cout << endl;
        cout << "Executing instruction: " << Program[prgmcountr] << endl;
    }
    else
    {
        cout << endl
             << "Executing instruction: " << Program[prgmcountr - 1] << endl;
    }
    cout << endl;
    cout << "Program Counter: " << (4 * prgmcountr) << endl;
    cout << endl;
    cout << "Registers:" << endl
         << endl;
    // cout << "Register"
    //<< "    "
    //<< "Value"
    //<< "     "
    //<< "Register"
    //<< "    "
    //<< "Value";
    for (int32_t i = 0; i < 16; i++)
    {
        cout << Registers[i].c_str() << "     " << i << "     " << RegValues[i] << endl;
    }
    for (int32_t i = 0; i < 16; i++)
    {
        cout << Registers[i + 16].c_str() << "     " << i + 16 << "     " << RegValues[i + 16] << endl;
    }
    for (int32_t i = 0; i < mem.size(); i++) // labels
    {
        cout << 40400 + 4 * i << "    " << mem[i].label.c_str() << "     " << mem[i].val << endl;
    }
    cout << endl;
}
void Simulator::assnum(string s)
{
    for (int32_t j = 0; j < s.size(); j++)
    {
        if (j == 0 && s[j] == '-')
        {
            continue;
        }
        if (s[j] < 48 || s[j] > 57)
        {
            cout << "Error: Specified value is not a number" << endl;
            error();
        }
    }
    if (s[0] != '-' && (s.size() > 10 || (s.size() == 10 && s > "2147483647")))
    {
        cout << "Error: Number out of range" << endl;
        error();
    }
    else if (s[0] == '-' && (s.size() > 11 || (s.size() == 11 && s > "-2147483648")))
    {
        cout << "Error: Number out of range" << endl;
        error();
    }
}
void Simulator::findreg(int32_t num)
{
    int32_t findregr = 0;
    if (curr_Inst.size() < 2)
    {
        cout << "Error: Register expected" << endl;
        error();
    }
    string regid = curr_Inst.substr(0, 2);
    if (curr_Inst.size() >= 4)
    {
        regid += curr_Inst.substr(2, 2);
    }
    for (int32_t i = 0; i < 32; i++)
    {
        if (regid == Registers[i])
        {
            reg[num] = i;
            findregr = 1;
            curr_Inst = curr_Inst.substr(2);
        }
    }
    if (findregr == 0)
    {
        cout << "Error: Invalid register" << endl;
        error();
    }
}
string Simulator::findlabel()
{
    removespaces(curr_Inst);
    string curr_string = "";
    int32_t findvalue = 0;
    int32_t doneFinding = 0;
    for (int32_t j = 0; j < curr_Inst.size(); j++)
    {
        if (findvalue == 1 && (curr_Inst[j] == ' ' || curr_Inst[j] == '\t') && doneFinding == 0)
        {
            doneFinding = 1;
        }
        else if (findvalue == 1 && curr_Inst[j] != ' ' && curr_Inst[j] != '\t' && doneFinding == 1)
        {
            cout << "Error: Unexpected text after value" << endl;
            error();
        }
        else if (findvalue == 0 && curr_Inst[j] != ' ' && curr_Inst[j] != '\t')
        {
            findvalue = 1;
            curr_string = curr_string + curr_Inst[j];
        }
        else if (findvalue == 1 && curr_Inst[j] != ' ' && curr_Inst[j] != '\t')
        {
            curr_string = curr_string + curr_Inst[j];
        }
    }
    return curr_string;
}

void Simulator::assremcomma()
{
    if (curr_Inst.size() < 2 || curr_Inst[0] != ',')
    {
        cout << "Error: Comma expected" << endl;
        error();
    }
    curr_Inst = curr_Inst.substr(1);
}

void Simulator::stack_bounds(int32_t in)
{
    if (!(in <= 40396 && in >= 40000 && in % 4 == 0))
    {
        cout << "Error: Invalid address for stack pointer. To access data section, use labels instead of addresses" << endl;
        error();
    }
}

void Simulator::asslaball(string s)
{
    if (s.size() == 0 || (s[0] > 47 && s[0] < 58))
    {
        cout << "Error: Invalid label" << endl;
        error();
    }
    for (int32_t i = 0; i < s.size(); i++)
    {
        if (!((s[i] > 47 && s[i] < 58) || (s[i] >= 65 && s[i] <= 90) || (s[i] >= 97 && s[i] <= 122)))
        {
            cout << "Error: Invalid label" << endl;
            error();
        }
    }
}
int32_t sortmem(memory a, memory b)
{
    return a.label < b.label;
}

int32_t sortreg(Labels a, Labels b)
{
    return a.label < b.label;
}
int main()
{
    string path;
    int32_t m;
    cout << "-----------------------------------" << endl;
    cout << "RISC V Simulator" << endl;
    cout << "-----------------------------------" << endl;
    cout << "Enter the path : ";
    cin >> path;
    cout << "Enter the mode type : ";
    cin >> m;
    if (m != 1 && m != 2)
    {
        cout << "Error : Mode is invalid" << endl;
        return 1;
    }
    Simulator simulator(m - 1, path);
    simulator.execute();
    return 0;
}