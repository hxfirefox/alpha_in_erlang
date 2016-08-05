# 参加邓辉解构Alpha Go培训班

## 从对弈出发

早期采用内嵌专家系统处理对弈，如[Deep Blue](https://en.wikipedia.org/wiki/Deep_Blue_(chess_computer))，通过对每步棋的估值作出选择，而Alpha Go则不同，其原理是通过大量模拟获得落子位置获胜的数学统计，进而逼近最优落子位置，实现在不依赖围棋定势基础上下围棋。

具体做法可简化为采用随机选取当前盘面上的任意点下一手棋，并模拟随后步骤直至棋局结束，根据胜负情况记录所下的此手标记该手棋。

>**Tips:**
>建模过程中是对数据的定义
>
>对并发模型的一些思考

## [Monte Carlo method](https://en.wikipedia.org/wiki/Monte_Carlo_method)
From Wikipedia, the free encyclopedia

**Monte Carlo methods** (or **Monte Carlo experiments**) are a broad class of computational algorithms that rely on repeated random sampling to obtain numerical results. Their essential idea is using randomness to solve problems that might be deterministic in principle. They are often used in physical and mathematical problems and are most useful when it is difficult or impossible to use other approaches. Monte Carlo methods are mainly used in three distinct problem classes:[1] optimization, numerical integration, and generating draws from a probability distribution.

Monte Carlo methods vary, but tend to follow a particular pattern:

- Define a domain of possible inputs.
- Generate inputs randomly from a probability distribution over the domain.
- Perform a deterministic computation on the inputs.
- Aggregate the results.

For example, consider a circle inscribed in a unit square. Given that the circle and the square have a ratio of areas that is π/4, the value of π can be approximated using a Monte Carlo method:

- Draw a square on the ground, then inscribe a circle within it.
- Uniformly scatter some objects of uniform size (grains of rice or sand) over the square.
- Count the number of objects inside the circle and the total number of objects.
- The ratio of the two counts is an estimate of the ratio of the two areas, which is π/4. Multiply the result by 4 to estimate π.

计算π的例子见文件src/mc_pi.erl

>**Tips:**
>
>Monte Carlo Tree Search，按集合空间理解，而非树

## Mathematics

站在编程语言之外看待语言，诸如汇编、C++、Java、Erlang等，其本质都是数据，它们之间不相同的是模型——要解决的问题模型，这导致了语义层次的差异，同时也揭示了解决问题的途径——提升抽象层次，语义层次。

引用(《Category Theory for Programmers》)[https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/]的原文说明上述理论：

>Changes in hardware and the growing complexity of software are forcing us to rethink the foundations of programming. Just like the builders of Europe’s great gothic cathedrals we’ve been honing our craft to the limits of material and structure. There is an unfinished gothic cathedral in Beauvais, France, that stands witness to this deeply human struggle with limitations. It was intended to beat all previous records of height and lightness, but it suffered a series of collapses. Ad hoc measures like iron rods and wooden supports keep if from disintegrating, but obviously a lot of things went wrong. From a modern perspective, it’s a miracle that so many gothic structures had been successfully completed without the help of modern material science, computer modelling, finite element analysis, and general math and physics. I hope future generations will be as admiring of the programming skills we’ve been displaying in building complex operating systems, web servers, and the internet infrastructure. And, frankly, they should, because we’ve done all this based on very flimsy theoretical foundations. We have to fix those foundations if we want to move forward.

![img=gothic](https://bartoszmilewski.files.wordpress.com/2014/10/beauvais_interior_supports.jpg)

## Thinking

### 如何做到探索与利用现有成果平衡

**Multi-armed bandit**

From Wikipedia, the free encyclopedia

In probability theory, the multi-armed bandit problem (sometimes called the K or N-armed bandit problem) is a problem in which a gambler at a row of slot machines (sometimes known as "one-armed bandits") has to decide which machines to play, how many times to play each machine and in which order to play them.When played, each machine provides a random reward from a probability distribution specific to that machine. The objective of the gambler is to maximize the sum of rewards earned through a sequence of lever pulls.

### 如何用并行的方式，缩小宽度和深度

见src/mcts_p.erl及src/mcts_ucb1_p.erl

## Alpha Go

- mcts 框架 + policy network value network

> 监督式的学习——分类，带答案的问题(人类盘面的走法)，基于前面的学习作答未知问题，policy network 宽度
>增强式的学习——通过反馈来刺激学习，追求结果 value network 深度
>监督式的学习——回顾分析