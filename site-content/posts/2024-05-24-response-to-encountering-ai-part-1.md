<!-- -*- fill-column: 200 -*- -->

--
title: "A response to 'Encountering Artificial Intelligence' - Part 1"
author: "Travis Athougies
tags: "ai","ethics"
published: false
---

Well, the Vatican published a book on AI: [Encountering Artificial Intelligence: Ethical and Anthropological
Investigations](https://jmt.scholasticahq.com/article/91230-encountering-artificial-intelligence-ethical-and-anthropological-investigations). It's a well laid out book, with good insights, as one
might expect from expert scholars and theologians.

What's prompted the book, of course, is the meteoric rise of large language models (and probably diffusion-based generative audio-visual models like StableDiffusion), which have turned the long AI
winter of the 90s, 2000s, and early 2010s, into a sweltering summer of AI. As the book says, ChatGPT grew to more than a 100 million users in its first week of publication. That's... a lot of people,
so it's no wonder the ethical implications behind AI have captured the attention of many disparate groups. As the book states, AIs have already convinced couples to divorce and people to commit
suicide. Clearly, someone needs to be talking about ethics at this point.

Naturally, this book approaches AI from a Catholic perspective, and in that sense, I think it achieves its goals. Here are my notes as I'm reading through the book (still not done yet, so things might
change).

# This is not your father's computing

One of the first things the book does is define its terms. On page 18, the authors write:

> A computation is "the transformation of sequences of symbols according to precise rules." This set of precise rules or “recipe for solving a specific problem by manipulating symbols” is called an algorithm.

It then defines:

> Machine learning (ML) is a computational process and method of analysis by which algorithms make inferences and predictions based on input data

They're not *wrong*, but I'm not sure they're *right* either. A lot of common discourse today talks about 'algorithms', these abstract ever-present entities of pure logic that run our lives. But what
is an algorithm really? I think the authors basically have it correct. An algorithm is a set of precise rules to solve a *specific* problem by manipulating *symbols*. In this way, algorithms are very
useful to us and predate computing. For example, multi-digit multiplication is a specific algorithm that we all know.

Now certainly, many machine learning problems, such as regressions and linear programming and some clustering methods, involve algorithms. However, I think the book fails to contend with the fact that
these algorithms are not what people talk about when we mean AI. None of these methods (or their related algorithmic cousins, expert systems) have been able to achieve anything close to ChatGPT.

Algorithms like the ones above can be analyzed, broken down, and understood. Modern AI is based, as the book identifies, on the notion of deep neural networks. Without going into too much technical
detail, essentially they take as input a large array of numbers and output another large array of numbers  (thrilling, I know). What happens in between is pretty much a mystery.

Now when I say a 'mystery' I don't mean that we don't know what's going on. Obviously, we do. Given that this is what I earn my living off of, I certainly know all the operations taking place in
minute detail (or at least my boss thinks I do!). What we don't know is what any of the numbers *mean*. The numbers that govern the internal processing are derived not from any description of an
algorithm, but from a learning process. Again, the learning process is similarly a mystery. While we know every step of how to run the process, we don't know really understand it. For example, the
graph below is the training loss of various BERT models. As loss decreases the model is perfoming better. Notice that at some points in training, the loss doesn't move at all. We repeat the same
training process, but nothing changes, until finally, the loss drops drastically in a short period of time, almost as if the model 'discovered' something. Yet, ask any machine learning researcher what
this all means, and you'll get blank stares or overconfident responses. No one knows. It's not for lack of trying either; there have been many papers explaining various aspects [^1] [^2], but there's
no widespread consensus [^3].

[^1] [*How Do Transformers Learn Topic Structure: Towards a Mechanistic Understanding*, Li, Yuchen and Li, Yuanzhi and Risteski, Andrej,Proceedings of the 40th International Conference on Machine Learning](https://proceedings.mlr.press/v202/li23p.html)

[^2] [*Transformers Learn In-Context by Gradient Descent*, Krause, Andreas and Brunskill, Emma and Cho, Kyunghyun and Engelhardt, Barbara and Sabato, Sivan and Scarlett, Jonathan,Proceedings of Machine Learning Research](https://proceedings.mlr.press/v202/von-oswald23a.html)

[^3] For example, [*Revisiting the Hypothesis: Do pretrained Transformers Learn In-Context by Gradient Descent?*](https://arxiv.org/abs/2310.08540) completely disagrees with [^2].

![BERT loss graphs. From *CoRe: An Efficient Coarse-refined Training Framework for BERT* by Yang et al](image:ai-book/bert-graph.ppm)

Essentially, while we understand what to do here, we don't understand deeply why it works. This is a lot different than long multiplication for example. It's trivial to explain that algorithm. It
relies on the distributed property of multiplication and the definition of place value. Every step is justifiable as to why it results in the correct answer. We've developed very good mechanisms
[^coq] to mechanically not only do multiplication but also to *prove* that following these steps will lead to the correct answer. Nothing similar exists for neural network models.

[^coq] For example, see theorem provers like [Coq](https://coq.inria.fr/), [Lean](https://leanprover-community.github.io/), or [Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php).

Many parents with kids who are bad sleepers have a bedtime routine. Sometimes, these routines are quite elaborate. My daughter, for example, demands I rub her feet, read her specific stories, and get
the lighting levels just right. Again, I understand every step of this process, but God help me if you ask me to explain why this works (if you know why this works, please tell me... my wife and I
would be thrilled). This is basically what we're doing with AI, and we've discovered how to do it in the same way -- it's mostly trial and error with a few guiding principles.

# Sir, this is a Wendy's

So if neural networks are not *algorithms* what are they? Well, again, I think we have to be careful with terms. Neural networks (or really, transformers, in the case of GPT) are algorithms. However,
the models that they run (meaning the weights learned during the learning process, combined with the algorithm to execute them) are not. This distinction really matters because it tells us what we can
and cannot say about the neural network algorithm.

Okay, I'm going to go back into deep computer science here, so everyone hold on tight.

Imagine a grid of cells. Each cell is either *dead* or *alive*. Each cell has eight neighbors. I'm going to give the following rules to update the cell:

1. If a cell is dead, and the cell has three live neighbors, then make the cell alive in the next round (*birth rule*).
2. If a cell is alive, and has 0 or 1 neighbors, then it dies. Similarly, if it is alive and has more than 4 neighbors, it dies (*death rule*).
3. If a cell is alive and has 2 or 3 live neighbors, then it is alive (*survival rule*).

Easy right? We can understand this completely. If I give you a piece of graph paper with some cells filled in to signal they're alive and some blank to show they're dead, you can apply these rules for
as long as I ask, and give me the result. In fact, we can program computers to do this [^life].

Okay so the truly great thing about these three rules, is that they fully encapsulate every possible computation that mankind can do. You can arrange alive or dead cells to do almost anything like
calculate pi [^pi], make a computer [^computer-life], or even simulate itself [^life-itself].

This system is called Conway's Game of Life. And here's the thing, while we fully understand how to run Conway's Game of Life (just follow the rules above), we actually can't say a whole lot about
it. There are useful patterns, like the ones above, and then there are some which we really can't say anything about. They may do something, they may not. It's not only unknown what they do, it's
*unknowable* and maybe even *indescribable* [^halting-problem]. Again, this is something we simply do not know.

We understand the *meta-algorithm* completely, but we cannot understand the behavior with just any input.

[^halting-problem] I cannot stress how truly unknowable this is. It is not simply difficult, or time-consuming, there are statements about various configurations that we *can never* know for
certain. This is the [halting problem.](https://en.wikipedia.org/wiki/Halting_Problem).

Okay, computer science over.

This is really similar to AI. We understand the rules of the network (the *algorithm*) completely. For GPT, it's a deep transformer decoder-only model with multi-headed softmax-attention, an embedding
step and a linear projection into the final domain (phew!). Given a description like this, I can easily write a program that does the computation step. However, this is not ChatGPT. ChatGPT is this
*plus* the weights trained by OpenAI. And, when you do the computation (the algorithm) with the weights, you get something that seems much more advanced than a long sequence of additions,
multiplications, exponentiations, and divisions. Like... a lot more advanced.

# A bunch of rocks

Okay, so for the first part of this, I'm going to end here. I think ultimately that there is an insiduous category mistake in simply talking AI in terms of algorithms. There are algorithms
underpinning AI (the model structure), but this algorithm is not what people talk about when they say AI. AI is the model plus the weights, much like Conway's game of life is the three rules, plus the
initial starting configuration. If you leave the weights out the algorithm shows nothing that could possibly be said to be intelligent. All zero model weights return the same answer for any input.

I am not saying that AI models will reach the same fate as the Game of Life in terms of transcending human ability. I drew the analogy to Conway's game of life because I think it's important to
realize that even simple algorithms can, when put together, produce results that defy the abilities of human cognition. Just because something is algorithmic does not make it characterizable. While
Conway's Game of Life is deterministic (it's never subject to random chance), it is actually not determinable. Something being algorithmic does not make it understandable.

This is an important distinction. On page 59, the authors say:

> Definitions of reason and intelligence commonly applied to AI abandon this interiority in favor of a twofold reduction. First, rationality and understanding become the logical manipulation of
> symbolically represented information; and second, intelligence becomes efficacious problem-solving

Given the path laid out above, it's apparent that there is nothing reductive about symbolic manipulation. As we've seen, manipulation of symbols, while simple, provably leads to things which truly
only Divinity can understand. And secondly, given that, it's not at all obvious why it would at all be bad if intelligence were to be 'merely' efficacious problem-solving. That 'merely' is doing a lot
of work there, which, like the symbolic manipulation, may be easy to write, but containing an entire universe of things to dissect.

# Nothing really matters

Now, I don't think this means we should dismiss the book. The authors take great care to define these terms, but the rest of the book doesn't rely on it as much as they think. The sections on ethics
and the issues with both utilitarianism and princple-based ethics do fine on their own. Their explorations and what constitutes a person and intelligence also do not rely much on this, but you have to
filter out some of the statements made to get at the meat of things. However, the book would do well to abandon this particular dismissal of symbolic manipulation, because focusing on it makes the
book easily glossed over by proponents of consciousness-as-emergent-phenomenon.
