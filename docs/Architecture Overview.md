流水线设计：动态流水线
取指1-取指2-指令队列-译码/重命名/分发-发射队列-读操作数-执行-退休-提交
解码宽度2，最大发射宽度5(ALU0, ALU1, MULU, DIVU, LSU)
异常处理/分支预测失败处理/CSR指令等均在退休时处理，清空后续流水线
分支预测暂时实现为Not Taken以减少工作量，但预留接口
# 功能部件
## I-Cache
可配置，两级流水化，VIPT，默认4路组相联，8KB，块大小64字节，伪LRU替换，不支持Outstanding miss；
每周期取出的2条指令可在不同块中，但必须在同一页中
## D-Cache
可配置，两级流水化，VIPT，默认4路组相联，8KB，块大小64字节，伪LRU替换，支持1 Outstanding miss；
Write Buffer为大小为8的FIFO，由4个指针enqPtr、retPtr、reqPtr、deqPtr控制，表项包含Cache块的起始物理地址、写入的数据、字节为单位的块内写掩码
对于LL Bit的实现，处理器维护一个全局的LL Bit(由LLBCTL CSR提供)和其地址，在LL.W指令和满足条件的ERTN执行时清空流水线重新执行。SC.W指令在译码时即根据译码时的LL Bit被译码为存数写1/写0指令，但始终被分发到LSU中执行，执行时使用Load指令的微操作完成。

## TLB
4项全相联，对体系结构可见
## BPU
BPU采用多级机制：Next Line Predictor用于迅速生成Next PC，Full Predictor流水化给出高准确度预测
### Next Line Predictor
目前阶段实现为All Not Taken策略
### Full Predictor
目前阶段实现为All Not Taken策略
## FU
### ALU
组合逻辑，处理算术、分支跳转、CSR/RDCNT指令，单拍完成
立即数由src2读取，CSR/Stable Counter操作数由src1读取
ALU0可以执行CSR读写指令，写结果由ALU0独有的外挂XCHG模块产生，被送到全局CSR Buffer中，在退休时更新CSR；CSR Buffer在流水线清空时解锁，在写入时上锁，以消除WAW导致的意外更新问题
ALU1可以执行CSR读/Stable Counter读指令
### MULU
3段流水华莱士树
### DIVU
移位除法
### AGU/LSU
包含Miss Buffer，可在1 Uncached/Cache miss的情况下继续处理已缓存的访存操作
## 指令队列
FIFO，每周期最多送出2条指令。在出队时对指令进行预译码，得出指令分发的目标FU。
## IDU
并行布置两个完全相同的单元同时解码两条指令，纯组合逻辑，将送入的指令译码为uop送FU发射队列
## Renamer/Dispatcher
根据预译码结果：
1.从Free List中获取空闲寄存器并根据分发结果修改sRAT；
2.查询对应FU的发射队列并将IDU译码结果、分配的目的寄存器等送发射队列
## 发射队列
根据操作数有效情况选择队列内最老指令发射；采用压缩结构，最深的指令永远是最老的
发射队列需对PRF的写回广播和提前唤醒信号进行监听，并在下一周期更新指令操作数的有效状态
## ROB
FIFO，32项，每项包含的信息有：
PC，用于异常处理和调试
ARD，体系结构目的寄存器，用于调试
PRD，物理目的寄存器，用于标记指令完成、更新aRAT
pPRD，体系结构目的寄存器先前对应的物理目的寄存器，用于更新Free List。注意这个信息不能从aRAT中取得，在同时退休的WAW指令下aRAT信息不可靠，会丢空闲寄存器
specialOP，指令在退休时需进行的额外操作，比如唤醒Write Buffer、更新分支预测器等
status，指令当前状态，分InProgress、Interrupt、Exception、BranchFail、Complete五种
## RAT
### sRAT/aRAT
32项，每项按索引编号对应一个体系结构寄存器，存储对应的PRF寄存器号
### Free List
FIFO，存放空闲未被分配的PRF寄存器号
## PRF
64项
## 转发网络
两个ALU的结果向本身/之间、访存流水线转发，访存流水线的结果向本身和ALU、MULU转发，MULU的结果向访存流水线转发
转发网络从FU的输出端口直接获取值转发到读操作数阶段，不设Result Drive段
# 流水线段操作
级间使用Ready/Valid信号进行握手。
## 取指1
利用PC寄存器中的值并行访问TLB、I-Cache，根据Next Line Predictor的输出取得Next PC送Next PC选择逻辑
Next PC选择逻辑根据情况从Next Line Predictor、Full Predictor、Actual PC中选取合适值送PC寄存器
I-Cache根据Index进行数据和Tag的读取，I-TLB进行地址翻译
## 取指2
TLB完成地址翻译，送I-Cache进行Tag比较；判断TLB相关异常；I-Cache进行Tag比较并依据比较结果选路输出送指令队列/阻塞填充；并行进行Full Predictor的结果检查，若结果不一致则根据Full Predictor结果刷新取指1段
注意Corner Case：在Full Predictor预测与Next Line Predictor结果不一致时，若Cache miss则不发出Cache Refill请求，即比较结果被用于控制(实现时注意查AXI手册确定Ready/Valid哪一个不可被撤销！)
## 指令队列
每周期送最多两条指令到译码/重命名/分发级，在指令出队时进行预译码，得出指令分发的目标FU和目的寄存器
## 译码/重命名/分发
并行进行三种操作：
1.根据指令出队时得到的预译码信息，向各发射队列分发指令；
NOTE: 每个FU的发射队列每周期最多写入1条指令。当因为发射队列满等因素指令无法全部分发时，若被阻塞的指令为两条指令中顺序上的第1条，则为保证指令在ROB中顺序写入，两条指令均不分发，流水线被阻塞；若被阻塞的指令为两条指令中顺序上的第2条，则分发第1条指令并阻塞流水线。
2.两个独立译码单元对出队的两条指令进行译码，并将译码结果送分发到的FU发射队列
3.根据当前分发的指令数目、指令出队时得到的预译码信息尝试在ROB中分配表项、从Free List获取空闲寄存器并修改sRAT，分配结果同步作为分发成功与否的判断依据
## 发射队列
根据操作数准备情况选出最老的就绪指令发射；根据推测唤醒信号更新队列中指令的操作数就绪情况
注意级间阻塞：当Write Buffer将满(已发射的指令可能填满Write Buffer)时，LSU不再发射新指令，以防止死锁
### 提前唤醒
发射队列监听sRAT的写回广播和提前唤醒信号，并在下个周期将对应的操作数置为有效。
写回广播在指令退休时发出，同步修改sRAT；提前唤醒信号与转发网络配合，在以下条件下发出：
ALU指令在发射时唤醒ALU0、ALU1、访存流水线；
访存Load指令在访存1段唤醒ALU0、ALU1、访存流水线、MULU；
MULU指令在执行1段唤醒访存流水线
注意：对于访存Load指令的唤醒采用保守的推测策略以保证用于唤醒的指令一定不会被阻塞，维护最后访问的4个(由D-Cache相联度决定)Cache块虚拟地址，在发生Cache miss时清除地址，唤醒计算时利用AGU计算的虚拟地址进行匹配，当且仅当匹配成功且当前位于访存2段的指令未阻塞时发出唤醒信号。注意访存3段指令和提交段指令不会阻塞，不需考虑。
## 读操作数
来源选择逻辑根据转发源广播的目的寄存器号确定当前操作数是否来源于转发逻辑。若转发网络中没有广播对应的目的寄存器，则操作数从PRF中取得，否则从转发网络中取得
## 执行
### ALU指令
ALU指令单拍完成，负责处理乘除法之外的整数运算指令和分支跳转指令
分支跳转指令在ALU中验证预测结果是否正确，信号一路带至退休时处理
### 访存指令
访存指令需要至少3周期完成。
#### 访存1
Load指令：
AGU计算访存地址，送TLB进行地址翻译、送D-Cache进行第1级查询
Store指令执行时：
AGU计算访存地址，送TLB进行地址翻译
Store指令退休时：
Write Buffer送地址到D-Cache进行命中查询
Load指令/Store指令退休时：
若Miss Buffer中有正在AXI交互的指令，比较当前指令的index是否与Miss Buffer中指令的index相等；若相等则指令被阻塞在访存1段，以避免Miss Buffer重填Cache造成的访存2段Miss信息不准确问题
#### 访存2
Load指令：
D-Cache根据TLB翻译结果进行Tag匹配，判断TLB相关异常，查询Write Buffer
Store指令执行时：
相关信息送Write Buffer，判断TLB相关异常

> Store指令在执行时不检查命中情况，因为这些指令反正会进Write Buffer；在进Write Buffer到退休这段时间内Cache可能发生替换，原有的检查结果并不能帮助退休时的Store检查

Store指令退休时：
进行Tag匹配，合并写入
Cache miss/Uncached时(Load/Store指令退休时)：
立即将指令移入Miss Buffer并启动AXI交互，交互完成后按替换策略进行替换，同时释放Miss Buffer；若在Miss Buffer非空闲时再次发生Cache miss/Uncached，则指令阻塞在访存2段等待Miss Buffer空闲
#### 访存3
Load指令：
合并Cache和Write Buffer的查询结果，送出结果
Store指令(执行/退休时)：
NOP，退休Store在访存2段完成后即退休
### 乘法指令
#### 执行1
华莱士树第1段
#### 执行2
华莱士树第2段
#### 执行3
华莱士树第3段，送出结果
### 除法指令
阻塞执行，多周期
## 提交
将结果写回PRF，并更改sRAT、更新ROB
## 退休
当指令正确执行无异常时，更新aRAT，释放ROB表项、向Free List归还先前指令使用的物理寄存器，允许一次退休2条；
当CSR指令退休时，流水线被清空，CSR被更改，随后重新执行；
当异常/分支预测失败指令退休时，流水线被清空，分支预测器被更新，sRAT根据aRAT回滚