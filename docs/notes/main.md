
# Dims: A Platform for Decentralized Research & Collaboration

## Introduction

We are all constantly in the process of metabolizing information. Others have gone before us, taking the time to carefully construct artifacts that merge disparate sources and original ideas into whatever construction the author deemed worthy to publish. Now it's our turn to take these artifacts, pull from them, and produce artifacts of our own. This cycle continues over and over again, although there are different reasons why people would produce such artifacts, whether for publishing, learning, or fun. Also, people might have different procedures for accomplishing this task. Procedures may be followed on an individual basis, or as a group.

We take notes in our own words to structure our thoughts about concepts we haven't fully grasped. These may never be meant for the consumption of others, but exist along the path of our learning experience and help in the production of more-polished versions that could be iterated upon to prove useful to a wider audience. We concurrently break things down into meaningful parts, and test how those parts could fit together in different ways to produce new gestalts, more than the sum of their parts.

### A List of Features

Let's take apart the paragraphs in the previous section and create a list of semantically distinct features that should prove useful when describing how people might work together in different fields.

- **Information** : Meaningful data encoded somehow in a medium where it can be extracted and decoded later.
- **Artifact** : Any form through which information can be transmitted to others (or your future self). This could include text documents, videos, memories, songs, memes, spoken sentences, DNA, or anything else with the capacity to hold information.
- **Source** : Ideas come from somewhere. It's useful to be able to trace things back to where you found them!
- **(New) Idea** : Something you came up with on your own, though it's probably related to other things you learned from other places.
- **Metabolism** : Breaking things down into small, easily digestible parts for later use.
- **Procedures** : Different tasks might have different steps that are required to accomplish them.
- **Construction** : Building something new from the pieces that were previously digested. 
- **Iteration** : We need a process for tracking how one version of an artifact maps to another version. What changes were made, and why?

This list is far from exhaustive, and could be further refined into a smaller set of semantic concepts, but it's a good place to start for building up a theory of knowledge management and finding a minimal flexible set of primitives to define this task.

Please stay tuned for more details... Here's a list of things I'm currently working on:

- Refining the minimal set of semantic constructs that were discussed above.
- This site will eventually act as a web-based interface for anyone's public knowledge base, just post the `index.html` file wherever you want to serve the page from along with your `notes/` folder, and it should work as long as it's somewhere that can serve the markdown files in the folder through http requests.
- I'm working on a typed markup language with a simpler spec than Markdown, but more focused on minimal semantic constructions and type-safe composition of work. You should always be able to import Markdown, but a smaller, stricter format can be helpful in refining ideas.
- There is a lot to be said about the main application behind this project. Building up a connected knowledge graph of your ideas and imposing type constraints on document structures requires a stand alone application. There's a lot of category theory and distilling down the shared essence of different algorithms, along with some of the work described above that needs to be finished before a comprehensive spec for the main app can be produced.
