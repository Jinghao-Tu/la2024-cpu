流水线设计：动态流水线
取指1-取指2-指令队列-译码/重命名/分发-发射队列-读操作数-执行-提交-退休
解码宽度2，最大发射宽度5(ALU0, ALU1, MULU, DIVU, LSU)
异常处理/分支预测失败处理/CSR指令等均在退休时处理，清空后续流水线
分支预测暂时实现为Not Taken以减少工作量，但预留接口
# 功能部件
## I-Cache
可配置，两级流水化，VIPT，默认4路组相联，8KB，块大小64字节，伪LRU替换，不支持Outstanding miss；
每周期取出的2条指令可在不同块中，但必须在同一4KB划分中(对应支持的最小页大小，这样不用检查当前页大小)
## D-Cache
可配置，两级流水化，VIPT，默认4路组相联，8KB，块大小64字节，伪LRU替换，支持1 Outstanding miss。
Write Buffer为大小为8的FIFO，表项包含Cache字的index、选路信号、读出的写入前的数据、ROB索引、有效信号。当流水线刷新信号发出时，Write Buffer中的表项被倒序写回Cache，从而回退推测执行对Cache的更改。在回退期间LSU被阻塞。
Miss Buffer用于支持Outstanding Miss，存储有Cached信息（用于确定读写使用的Burst类型是WRAP还是INCR）、读写信息（用于确定操作是读还是写）、移位完成后的写入数据（对于Miss Cached和Uncached，这里恰好能够统一）、字节为单位的写掩码（也能够统一）、ROB索引、有效信号
对于LL Bit的实现，处理器维护一个全局的LL Bit(由LLBCTL CSR提供)和其地址，在LL.W指令和满足条件的ERTN执行时清空流水线重新执行。
推测唤醒信号由唤醒地址缓存管理：缓存大小为4，存储有近期访问过的Cached Cache Line的起始VA。AGU在访存1段计算出访存VA后即查询唤醒地址缓存，若命中则发出唤醒信号，在提交阶段进行转发。唤醒地址缓存在LRU信号更新的同时被更新，在非分支预测失败导致的流水线刷新时被清空，以确保“地址缓存命中则D-Cache一定命中”这一设计前提始终被满足。推理：在TLB映射关系发生改变时地址缓存需要被清空，而涉及流水线清空的操作有分支预测失败、LL指令退休、中断/异常、CSR指令、Cache/TLB维护指令退休、wait指令退休，其中只有分支预测失败、LL指令退休两种情况下TLB映射关系不会发生改变。对于性能测试而言，只需考虑分支预测失败情况下的地址缓存保持，由ROB提供信号即可。为简化实现，LL指令退休时唤醒地址缓存也被清空，这可能对Linux的锁操作不友好，是演示测试的可能性能优化点。
LSU的清空逻辑如下：
在退休逻辑发现流水线需清空的当周期（对于LSU，清空信号在下周期到达），可以退休的指令的唤醒信号被发出；下一周期，清空信号到达，访存1、2段流水线寄存器在这一周期收到刷新信号，Write Buffer状态机准备进入回滚模式，唤醒地址缓存被视情况清空，Miss Buffer若没启动交互则被清空
## TLB
4项全相联，对体系结构可见；负责所有虚存的翻译工作
按LA32R手册，TLB支持4K和4M两种页大小。
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
ALU1可以执行Stable Counter读指令
### MULU
3段流水华莱士树
### DIVU
移位除法
### AGU/LSU
包含Miss Buffer，可在1 Uncached/Cache miss的情况下继续处理已缓存的访存操作
## 指令队列
FIFO，每周期最多送出2条指令。在出队时对指令进行预译码，得出指令分发的目标FU、体系结构目的寄存器。
## IDU
并行布置两个完全相同的单元同时解码两条指令，纯组合逻辑，将送入的指令译码为uop送FU发射队列
## Renamer/Dispatcher
根据预译码结果：
1.从Free List中获取空闲寄存器并根据分发结果修改sRAT；
2.查询对应FU的发射队列并将IDU译码结果、分配的目的寄存器等送发射队列
重命名逻辑不应为0号寄存器分配新的物理寄存器。
## 发射队列
根据操作数有效情况选择队列内最老指令发射；采用压缩结构，最深的指令永远是最老的
发射队列需对PRF的写回广播和提前唤醒信号进行监听，并在下一周期更新指令操作数的有效状态
ALU0的发射队列同时只能存在一条CSR指令，ALU1的发射队列同时只能存在一条Counter读指令
## ROB
FIFO，32项，每项包含的信息有：
PC，用于异常处理和调试
ARD，体系结构目的寄存器，用于调试和更新aRAT
PRD，物理目的寄存器，用于更新aRAT
pPRD，体系结构目的寄存器先前对应的物理目的寄存器，用于更新Free List。注意这个信息不能从aRAT中取得，在同时退休的WAW指令下aRAT信息不可靠，会丢空闲寄存器
specialOP，指令在退休时需进行的额外操作，比如唤醒LSU SpecialOP Buffer、更新分支预测器等
isComplete，指示指令是否提交
branchResult，指示分支指令的执行结果，退休时依据此信息更新BPU
exceptionInfo，包含指令执行时产生的异常信息，用于在退休时更新CSR
除调试接口外，退休逻辑不应将0号寄存器的写进行实际写操作，包括更新aRAT和Free List。
## RAT
### sRAT/aRAT
32项，每项按索引编号对应一个体系结构寄存器，存储对应的PRF寄存器号
重命名逻辑和退休逻辑应保证不会修改r0寄存器的映射。
### Free List
FIFO，存放空闲未被分配的PRF寄存器号，项数等于PRF大小
Free List使用三指针结构：allocPtr负责读取Free List本周期将要分配出去的物理寄存器，freePtr负责写入退休逻辑本周期将要归还的物理寄存器，recoverPtr负责指示对应在流水线中尚未退休的、已分配的物理寄存器的起始。recoverPtr-allocPtr部分的寄存器被分配给了尚未退休的指令作为目的寄存器，allocPtr-freePtr部分的寄存器可供分配，而freePtr-recoverPtr部分的寄存器则属于已经退休尚未被覆盖的寄存器表项或单纯的垃圾信息。
三指针设计有效地降低了流水线清空逻辑的复杂度——异常时将allocPtr恢复到recoverPtr即可，不需根据ROB进行WALK Recover。
当存在目的寄存器的指令退休时，recoverPtr+1(这个步骤在一个周期内可能进行退休宽度次)，此时recoverPtr扫过的寄存器项一定是刚刚退休的指令的目的寄存器项，这是由重命名顺序进行、指令顺序退休决定的。
当allocPtr追上freePtr时，Free List空，不再接受请求。freePtr不可能追上recoverPtr，这是因为二者的增加条件相同，被归还的寄存器项在aRAT中由recoverPtr释放的项替代。当recoverPtr追上allocPtr时，ROB中没有未退休的写寄存器指令，退休逻辑保证不会进行aRAT、Free List的修改。因此，空逻辑只需直接进行allocPtr和freePtr的相等判断，而各指针也不需像通常的环形缓冲区实现那样加上一位用于区分满和空——设计逻辑保证了Free List不可能满。
接上段，对于宽指针而言(比如allocPtr宽度为译码宽度、freePtr宽度为退休宽度)，部分重叠是允许的，但部分重叠时对应操作受到限制，应保证不会产生完全重叠或范围穿越。实际上只需限制分发的指令数量即可，这一限制由分发逻辑的结构阻塞检查完成。
## PRF
64项
## 转发网络
两个ALU的结果向本身/之间、访存流水线转发，访存流水线的结果向本身和ALU、MULU转发，MULU的结果向访存流水线转发
转发网络从FU的输出端口直接获取值转发到读操作数阶段，不设Result Drive段
转发网络不应对目标为0号寄存器的结果进行转发。
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
每周期送最多两条指令到译码/重命名/分发级，在指令出队时进行预译码，得出指令分发的目标FU、目的寄存器和源寄存器
## 译码/重命名/分发
并行进行三种操作：
1.根据指令出队时得到的预译码信息，向各发射队列分发指令；
NOTE: 每个FU的发射队列每周期最多写入1条指令。当因为发射队列满等因素指令无法全部分发时，若被阻塞的指令为两条指令中顺序上的第1条，则为保证指令在ROB中顺序写入，两条指令均不分发，流水线被阻塞；若被阻塞的指令为两条指令中顺序上的第2条，则分发第1条指令并阻塞流水线。
2.两个独立译码单元对出队的两条指令进行译码，并将译码结果送分发到的FU发射队列
3.根据当前分发的指令数目、指令出队时得到的预译码信息尝试在ROB中分配表项、从Free List获取空闲寄存器并修改sRAT，分配结果同步作为分发成功与否的判断依据
分发时，若源寄存器或目的寄存器中存在r0寄存器，则在源寄存器中其不专门分配物理寄存器，寄存器号由Dispatcher分配0；在目的寄存器中其也不专门分配物理寄存器，写使能信号被置为无效。
## 发射队列
根据操作数准备情况选出最老的就绪指令发射；根据PRF写回信号、推测唤醒信号更新队列中指令的操作数就绪情况
本周期将要进入发射队列的指令的源寄存器写回信号、推测唤醒信号也需一并监听，否则操作数的就绪情况将出现不一致现象。推测唤醒信号和源寄存器写回信号需分别对待，推测唤醒信号仅收到的下周期有效，源寄存器写回信号收到后一直有效：这是因为进行推测唤醒的指令在退休阶段无法将结果传递给读操作数阶段的指令，若指令在允许提前唤醒的当周期未被选中，而是在下周期被选中，读取的操作数将是错误的。
注意级间阻塞：当Write Buffer将满(已发射的指令可能填满Write Buffer)时，LSU不再发射新指令，以防止死锁
LSU指令始终顺序发射。
### 提前唤醒
发射队列监听sRAT的写回广播和提前唤醒信号，并在下个周期将对应的操作数置为有效。
写回广播在指令退休时发出，同步修改sRAT；提前唤醒信号与转发网络配合，在以下条件下发出：
ALU指令在发射时唤醒ALU0、ALU1、访存流水线；
访存Load指令在访存1段唤醒ALU0、ALU1、访存流水线、MULU；
MULU指令在执行1段唤醒访存流水线
注意：对于访存Load指令的唤醒采用保守的推测策略以保证用于唤醒的指令一定不会被阻塞，维护最后访问的4个(由D-Cache相联度决定)Cache块虚拟地址，在发生Cache miss时清除地址，唤醒计算时利用AGU计算的虚拟地址进行匹配，当且仅当匹配成功且当前位于访存2段的指令未阻塞时发出唤醒信号。注意提交段指令不会阻塞，不需考虑。
## 读操作数
来源选择逻辑根据转发源广播的目的寄存器号确定当前操作数是否来源于转发逻辑。若转发网络中没有广播对应的目的寄存器，则操作数从PRF中取得，否则从转发网络中取得
中断信号在这一周期进行标记。每周期流经RO段的指令均根据当前有无未屏蔽待处理的中断判断是否标记中断，在退休时由退休逻辑进行处理。这是基于Uncached Load、CSR读写指令与中断处理的关系、CACOP指令的实现作出的决定（如果标记在退休段，Uncached Load可能会在发起交互后被冲刷；如果标记在分发段，在RO段的CSR读指令可能会读到待处理中断，但实际的中断跳转发生在其后；对于CACOP指令的长期执行，标记在退休段需要LSU引入中断抑制信号，增加复杂度）。
## 执行
### ALU指令
ALU指令单拍完成，负责处理乘除法之外的整数运算指令和分支跳转指令
分支跳转指令在ALU中验证预测结果是否正确，信号一路带至退休时处理
### 访存指令
访存指令需要至少2周期完成。
PRELD、DBAR指令被实现为NOP。
对于Cached Load/Store，LRU信息在指令离开LSU时更新。
#### 访存1
##### Load/LL/Store/SC
AGU计算访存地址，送TLB进行地址翻译、送D-Cache进行第1级查询
若Miss Buffer中有正在AXI交互的指令，比较当前指令的index是否与Miss Buffer中指令的index相等；若相等则指令被阻塞在访存1段，以避免Miss Buffer重填Cache造成的访存2段Miss信息不准确问题
##### CACOP
AGU计算访存地址，送TLB进行地址翻译、对code进行译码；将操作送SpecialOP Buffer
##### TLB/IBAR
翻译产生LSU微操作；将操作送SpecialOP Buffer
#### 访存2
##### Load
D-Cache根据TLB翻译结果进行Tag匹配，判断TLB相关异常
##### LL
D-Cache根据TLB翻译结果进行Tag匹配，判断TLB相关异常，将PA送LL Buffer
##### CACOP
检查TLB翻译结果是否产生异常及异常是否有效(仅查询索引方式下有效)
这里的VA可以是真正的VA也可以是索引VA。注意只依赖VA的Cache操作方式对IBAR这样的Cache操作指令效率很低：其一次只能操作一个行的一路。我们不讲武德：直接向I-Cache发出复位信号，将所有valid信号一次性无效。
##### Store
合并写入内容，D-Cache根据TLB翻译结果进行Tag匹配，判断TLB相关异常，更新Write Buffer
##### SC
合并写入内容，查询LL Bit并决定是否写Cache，进行Tag匹配并判断TLB相关异常，更新Write Buffer
##### IBAR
将操作送SpecialOP Buffer
##### Cache miss/Uncached时
指令在下一周期移入Miss Buffer，满足条件后启动AXI交互，交互完成后按替换策略进行替换，同时释放Miss Buffer；若在Miss Buffer非空闲时再次发生Cache miss/Uncached，则指令阻塞在访存2段等待Miss Buffer空闲
对于Miss Cached Load，若Write Buffer中没有相同index的项，则启动AXI交互进行Cache Fill/Refill，在AXI交互首字返回时（同时完成了Cache Line的替换）阻塞访存2段指令，更新LRU信息并流出LSU。这个判断条件确保了被替换的脏行不含推测执行的数据，否则Write Buffer中会有相同index的项；相同index的项只可能来自在其之前进入Write Buffer的【同index但不同tag且Hit的】Store指令，因为在其之后进入Write Buffer的Store指令不可能有相同index，这由访存1段防止读取错误TAG的阻塞机制保证。
对于Uncached Load/Store，其在下一周期进入Miss Buffer，当ROB允许指令退休时，即前序指令当周期退休且无中断待响应时，启动AXI交互，在AXI交互返回时流出LSU并提交。
#### 特殊指令的退休行为
##### LL
LL Buffer被唤醒，CSR中相关信息被更新为LL Buffer的值
##### CACOP
阻塞I-Cache和D-Cache、等待正在进行的AXI交互完成以及Write Buffer清空、根据SpecialOP译码结果将VA/索引送对应的Cache进行操作、解除Cache阻塞
##### TLB
阻塞I-Cache和D-Cache、等待正在进行的AXI交互完成以及Write Buffer清空、根据SpecialOP译码结果操作TLB、解除Cache阻塞
##### IBAR
阻塞I-Cache、D-Cache、等待正在进行的AXI交互完成以及Write Buffer清空、写回所有脏的D-Cache行(但是脏位不变)、无效I-Cache、解除Cache阻塞
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
当写CSR指令退休时，流水线被清空，CSR被更改，随后后续指令重新执行；
当异常/分支预测失败指令退休时，流水线被清空，分支预测器被更新，sRAT根据aRAT回滚
流水线清空时当周期清空LSU、Free List、sRAT之外的所有功能部件，第二周期刷新LSU、Free List、sRAT，以确保流水线清空当周期正常退休的指令正确产生效果。这时取回的新指令至多到达取指1段，延迟清空不会对指令执行造成影响；CSR当周期被更改，以确保下一周期取指1段的指令在地址翻译时行为正确；若有Cache/TLB指令退休，流水线将被锁定直到LSU完成对Cache/TLB状态的更改
对于IDLE指令，流水线将被锁定，直到接收到中断，中断被标记在IDLE指令的下一条指令上；对于其他指令，操作数读取逻辑负责标记中断，若当周期接收到中断，中断自可用当周期起标记在当周期流经RO段的所有指令上（中断优先级最高，它可以覆盖掉有异常指令的异常）。IDLE指令不会被分发到FU，从而其正常情况下永远不会出现执行完毕的情况，自然阻塞了流水线。