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

# Thinking
1. 如何做到探索与利用现有成果平衡
2. 如何用并行的方式，缩小宽度和深度
