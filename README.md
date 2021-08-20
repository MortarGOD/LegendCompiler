# CSC2021-HDU-WeaverGirlsDescendToWorld
2021 年全国大学生计算机系统能力大赛编译系统设计赛项目<br>
队伍学校：杭州电子科技大学<br>
队伍名称：织女下凡<br>
队伍成员：吴璋达、郭佳毅
## Compiler架构
这是一个 SysY 语言 ( 简化的 C 语言 ) 的编译器, 目标平台是树莓派 ( ARMv7 ) ( 32bit )<br>
Scanner → Parser → CodeGen<br>
0层IR, 共3895行; 三步到位, 极致的简洁, 飞一般的感觉
## 要你命三板斧性能优化系统
1.将可以直接算出来的常量表达式或数组偏移量用常量替代<br>
2.除法优化 ( 对除数为2的n次方、常数、寄存器三种情况有对应的处理方式 )<br>
3.目标代码层面使用沃兹基发明的 “ 扫两次 ” 寄存器分配算法
## 输入与输出
输入文件地址通过Infile传入, 输出文件地址通过Outfile传入
