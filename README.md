# MJr-compiler

MJr is a probabilistic programming language based on pattern-rewriting, heavily inspired by [MarkovJunior](https://github.com/mxgmn/MarkovJunior) by Maxim Gumin. This project provides a compiler for the MJr programming language, which compiles MJr programs to JavaScript, TypeScript or Python 3.

![image](https://user-images.githubusercontent.com/32057631/188284857-b152cc85-d08d-4be1-9844-6c81b2a6acea.png)

## Examples

- [Basic.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnQmFzaWMnIG1vZGVsCiMgaHR0cHM6Ly9naXRodWIuY29tL214Z21uL01hcmtvdkp1bmlvci9ibG9iL21haW4vbW9kZWxzL0Jhc2ljLnhtbAoKZ3JpZCBbQlddCm9uZTogW0JdIC0+IFtXXQo=,JavaScript,1,1,17,17,0): the simplest MJr program that does anything. Hello, World!
- [Backtracker.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnQmFja3RyYWNrZXInIG1vZGVsCiMgaHR0cHM6Ly9naXRodWIuY29tL214Z21uL01hcmtvdkp1bmlvci9ibG9iL21haW4vbW9kZWxzL0JhY2t0cmFja2VyLnhtbAoKZ3JpZCBbQlJXVV0KCnB1dCBbUl0gYXQgb3JpZ2luCgptYXJrb3Y6CiAgICBvbmU6IFtSQl0gLT4gW1dSXQogICAgb25lOiBbUlddIC0+IFtVUl0K,JavaScript,1,1,17,17,0): generates connected cave systems with pools, using only two rewrite rules.
- [BacktrackerCycle.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnQmFja3RyYWNrZXJDeWNsZScgbW9kZWwKIyBodHRwczovL2dpdGh1Yi5jb20vbXhnbW4vTWFya292SnVuaW9yL2Jsb2IvbWFpbi9tb2RlbHMvQmFja3RyYWNrZXJDeWNsZS54bWwKCmdyaWQgW0JSR1dBXQoKcHV0IFtSXSBhdCBvcmlnaW4KCm1hcmtvdjoKICAgIG9uZTogW1JCQl0gLT4gW0dHUl0KICAgIG9uZTogW1JHR10gLT4gW1dBUl0KCm9uY2U6IFtSXSAtPiBbV10Kb25jZTogW1dCV10gLT4gW1dBV10KYWxsOiBbQV0gLT4gW1ddCmFsbDogW0JCQi9CV0JdIC0+IFsuLi4vLkIuXQo=,JavaScript,1,1,19,19,0): generates a random cycle.
- [River.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnUml2ZXInIG1vZGVsCiMgaHR0cHM6Ly9naXRodWIuY29tL214Z21uL01hcmtvdkp1bmlvci9ibG9iL21haW4vbW9kZWxzL1JpdmVyLnhtbAoKZ3JpZCBbQldSVUdFXQoKb25jZTogW0JdIC0+IFtXXQpvbmNlOiBbQl0gLT4gW1JdCgpvbmU6CiAgICBbUkJdIC0+IFtSUl0KICAgIFtXQl0gLT4gW1dXXQoKYWxsOiBbUlddIC0+IFtVVV0KYWxsOiBbW1dSXV0gLT4gW0JdCgpAbGltaXQgMQphbGw6IFtVQl0gLT4gW1VVXQoKYWxsOiBbQlUvVUJdIC0+IFtVLi8uLl0KYWxsOiBbVUJdIC0+IFsuR10KCkBsaW1pdCAxMwpvbmU6IFtCXSAtPiBbRV0KCm9uZToKICAgIFtFQl0gLT4gWy5FXQogICAgW0dCXSAtPiBbLkddCg==,JavaScript,1,1,128,128,5): generates a river with grass growing around it.
- [BasicSnake.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnQmFzaWNTbmFrZScgbW9kZWwKIyBodHRwczovL2dpdGh1Yi5jb20vbXhnbW4vTWFya292SnVuaW9yL2Jsb2IvbWFpbi9tb2RlbHMvQmFzaWNTbmFrZS54bWwKCmdyaWQgW0JXRFBHUl0KCnB1dCBbV10gYXQgb3JpZ2luCgphbGw6IFtbV0RdQkJdIC0+IFsuLkRdCgpvbmNlOiBbV0JEXSAtPiBbUEdSXQoKQGxpbWl0IDIKb25lOiBbUkJEXSAtPiBbR0dSXQoKQGxpbWl0IDE1Cm9uZTogW0RdIC0+IFtXXQoKbWFya292OgogICAgb25lOiBbUkJXXSAtPiBbR0dSXQogICAgYWxsOgogICAgICAgIFtSQkRdIC0+IFtHR1JdCiAgICAgICAgW1BHR10gLT4gW0RCUF0gaWYgY291bnQgW1JCRF0gPiAwCg==,JavaScript,1,1,31,31,-1): plays a classic game of *Snake*, albeit not very well.
- [PacMan.mjr](https://kaya3.github.io/MJr/playground/#IyBBdXRob3I6IEFuZHJldyBLYXkKCmdyaWQgW0JXRFBZSUVST10KCiMgZHJhdyBtYXplCnB1dCBbUF0gYXQgb3JpZ2luCmFsbDogW1BCQl0gLT4gWy4uUF0KcHV0IFtXXSBhdCBvcmlnaW4KYWxsOiBbV0JQQlBdIC0+IFsuLi4uV10KcHV0IFtEXSBhdCBvcmlnaW4KYWxsOiBbREJQQlddIC0+IFsuLlcuRF0Kb25lOiBbREJQQkRCUC9CQkJCQkJCLy5CUEJQQlBdIC0+IFsuLlcuLi4uLy4uLi4uLi4vLi4uLi4uLl0KCkBsaW1pdCA1Cm9uZTogW0RCUEJEXSAtPiBbLi5XLi5dCmFsbDogW1BCUF0gLT4gWy5QLl0KYWxsOiBbRF0gLT4gW1ddCm9uZTogW0JCQkJCQi9CUEJXQlAvQkJCQkJCXSAtPiBbLi4uLi4uLy4uUFBQLi8uLi4uLi5dCgojIGFkZCBwbGF5ZXIgYW5kIGVuZW1pZXMKcHV0IFtZXSBhdCBvcmlnaW4KCkBsaW1pdCA0Cm9uZTogW1ddIC0+IFtPXQoKIyBwbGF5IQoKdW5pb24gWz9dID0gW1tXQl1dCgpzZXF1ZW5jZToKICAgICMgbWFyayB1bnNhZmUgbW92ZXMKICAgIGFsbDogWy5CQkIvW1JPXUI/Qi8uQkJCXSAtPiBbLi5JLi8uSS5JLy4uSS5dIGlmIGNvdW50IFtZXSA+IDAKICAgIAogICAgIyBhbGxvdyBwbGF5ZXIgdG8gYmFja3RyYWNrIGlmIHRoZXkgaGF2ZSBubyBzYWZlIG1vdmUKICAgIG9uZTogW0VdIC0+IFtCXSBpZiBjb3VudCBbWUI/XSA9PSAwCiAgICAKICAgICMgYWxsb3cgcGxheWVyIHRvIG1ha2UgYW4gdW5zYWZlIG1vdmUgaWYgdGhleSBoYXZlIG5vIHNhZmUgbW92ZQogICAgb25lOiBbSV0gLT4gW0JdIGlmIGNvdW50IFtZQj9dID09IDAKICAgIAogICAgIyBwbGF5ZXIgbW92ZXMKICAgIEBsaW1pdCAxCiAgICBhbGw6CiAgICAgICAgW1lCV10gLT4gW0JFWV0KICAgICAgICBbWUJCXSAtPiBbQkVZXSBpZiBjb3VudCBbWUJXXSA9PSAwCiAgICAgICAgW1tJRV1dIC0+IFtCXQogICAgCiAgICAjIGVuZW15IGVhdHMgcGxheWVyLCBpZiBwb3NzaWJsZQogICAgb25jZToKICAgICAgICBbUkJZXSAtPiBbQkJSXQogICAgICAgIFtPQlldIC0+IFtXQlJdCiAgICAKICAgICMgZW5lbWllcyBtb3ZlCiAgICBAbGltaXQgMQogICAgYWxsOgogICAgICAgIGxldCBhbGl2ZSA9IGNvdW50IFtZXSA+IDAKICAgICAgICBbUkJXXSAtPiBbQkRPXSBpZiBhbGl2ZQogICAgICAgIFtPQlddIC0+IFtXRE9dIGlmIGFsaXZlCiAgICAgICAgW1JCQl0gLT4gW0JEUl0gaWYgYWxpdmUKICAgICAgICBbT0JCXSAtPiBbV0RSXSBpZiBhbGl2ZQogICAgICAgIFtEXSAtPiBbQl0K,JavaScript,1,1,45,45,0): another classic game.
- [BasicDungeonGrowth.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnQmFzaWNEdW5nZW9uR3Jvd3RoJyBtb2RlbAojIGh0dHBzOi8vZ2l0aHViLmNvbS9teGdtbi9NYXJrb3ZKdW5pb3IvYmxvYi9tYWluL21vZGVscy9CYXNpY0R1bmdlb25Hcm93dGgueG1sCgpncmlkIFtCUkFDREddCnVuaW9uIFs/XSA9IFtbQlJdXQoKcHV0IFtSXSBhdCBvcmlnaW4KCm9uZTogWy4uPy4uLy5CQkIuLy5CQkI/Ly5CQkIuLy4uUi4uXSAtPiBbQUFSQUEvQUREREEvQURERFIvQUREREEvQUFDQUFdCm9uZTogW0FDQS9CQkJdIC0+IFtBUkEvQkJCXQoKYWxsOiBbQ10gLT4gW0RdCmFsbDogW1JdIC0+IFtEXQoKYWxsOiBbQkRdIC0+IFsuQV0KYWxsOiBbREREL0FEQS9ERERdIC0+IFsuLi4vRC5ELy4uLl0KYWxsOiBbREREL0RBRC9ERERdIC0+IFsuLi4vLkQuLy4uLl0K,JavaScript,1,1,47,47,-2): builds a connected dungeon with rooms and corridors.
- [NystromDungeon.mjr](https://kaya3.github.io/MJr/playground/#IyBUcmFuc2xhdGVkIGZyb20gTWF4aW0gR3VtaW4ncyAnTnlzdHJvbUR1bmdlb24nIG1vZGVsLCB3aGljaAojIGlzIGJhc2VkIG9uIEJvYiBOeXN0cm9tJ3MgZHVuZ2VvbiBnZW5lcmF0aW9uIGFsZ29yaXRobQojIGh0dHBzOi8vZ2l0aHViLmNvbS9teGdtbi9NYXJrb3ZKdW5pb3IvYmxvYi9tYWluL21vZGVscy9OeXN0cm9tRHVuZ2Vvbi54bWwKIyBodHRwOi8vam91cm5hbC5zdHVmZndpdGhzdHVmZi5jb20vMjAxNC8xMi8yMS9yb29tcy1hbmQtbWF6ZXMvCgpsZXQgTlVNX0NZQ0xFUyA9IDUKCmdyaWQgW0JQV1JHXQoKIyBkcmF3IGEgZ3JpZApwdXQgW1BdIGF0IG9yaWdpbgphbGw6IFtQQkJdIC0+IFsuLlBdCgpsZXQgcm9vbUluID0gWwogICAgUEJQQlBCUEJQIC8KICAgIEJCQkJCQkJCQiAvCiAgICBQQlBCUEJQQlAgLwogICAgQkJCQkJCQkJCIC8KICAgIFBCUEJQQlBCUCAvCiAgICBCQkJCQkJCQkIgLwogICAgUEJQQlBCUEJQCl0KbGV0IHJvb21PdXQgPSBbCiAgICBXV1dXV1dXV1cgLwogICAgV1dXV1dXV1dXIC8KICAgIFdXV1dXV1dXVyAvCiAgICBXV1dXV1dXV1cgLwogICAgV1dXV1dXV1dXIC8KICAgIFdXV1dXV1dXVyAvCiAgICBXV1dXV1dXV1cKXQoKb25lOiByb29tSW4gLT4gcm9vbU91dAoKIyBnZW5lcmF0ZSBjb3JyaWRvcnMgd2l0aCBNYXplQmFja3RyYWNrZXIKbWFya292OgogICAgb25lOiBbUkJQXSAtPiBbR0dSXQogICAgb25lOiBbR0dSXSAtPiBbUldXXQogICAgb25lOiBbUF0gLT4gW1JdCgojIGxlYXZlIG9ubHkgb25lIHN0YXJ0Cm9uY2U6IFtSXSAtPiBbR10KCiMgZm9yZ2V0IG90aGVyIHN0YXJ0cwphbGw6IFtSXSAtPiBbV10KCiMgY29ubmVjdCBjb21wb25lbnRzCm1hcmtvdjoKICAgIGFsbDogW0dXV10gLT4gWy4uR10KICAgIG9uZTogW0dCV10gLT4gWy5XR10KCiMgaW5zZXJ0IGN5Y2xlcwpAbGltaXQgTlVNX0NZQ0xFUwpvbmU6IFtHQkddIC0+IFsuVy5dCgojIGZvcmdldCBzdHJ1Y3R1cmUKYWxsOiBbR10gLT4gW1ddCgojIHJldHJhY3QgZGVhZCBlbmRzCmFsbDogW0JCQi9CV0JdIC0+IFtCQkIvQkJCXQo=,JavaScript,1,1,39,39,0): builds a connected dungeon using [Bob Nystrom's algorithm](http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/).
- [MazesAndLakes.mjr](https://kaya3.github.io/MJr/playground/#IyBBdXRob3I6IEFuZHJldyBLYXkKCnVzZSBsZXQgZyA9IGdyaWQgW0JXUkVJXQoKbGV0IExBS0VfU0VFRFMgPSA0CmxldCBMQUtFX1NJWkUgPSBnLndpZHRoICogZy5oZWlnaHQgLy8gNApsZXQgTEFORF9TRUVEUyA9IDMyCmxldCBBTklNQVRFX1dBVEVSID0gdHJ1ZQoKIyBtYWtlIGEgZmV3IGxha2VzIGJ5IHJhbmRvbSBncm93dGgKQGxpbWl0IExBS0VfU0VFRFMKb25lOiBbQl0gLT4gW0ldCgpAbGltaXQgTEFLRV9TSVpFIC0gTEFLRV9TRUVEUwpvbmU6IFtJQl0gLT4gWy5JXQoKIyBtYWtlIHNvbWUgbGFuZCBieSBhIHNlbGYtYXZvaWRpbmcgcmFuZG9tIHdhbGsgd2l0aCBiYWNrdHJhY2tpbmcKQGxpbWl0IExBTkRfU0VFRFMKb25lOiBbQl0gLT4gW1JdCgptYXJrb3Y6CiAgICBvbmU6IFtSQkJdIC0+IFtXV1JdCiAgICBvbmU6IFtSV1ddIC0+IFtFRVJdCgpvbmU6IFtSXSAtPiBbRV0KCiMgZXJvZGUgbmFycm93IHNlY3Rpb25zIG9mIGxhbmQKb25lOiBbQkJXQkJdIC0+IFsuLkIuLl0KCiMgcmVwbGFjZSB0aGUgc29saWQgbGFrZXMgd2l0aCBpc29sYXRlZCBwaXhlbHMKQGxpbWl0IExBS0VfU0laRSAvLyAyCm9uZTogW0lJXSAtPiBbQkJdCgptYXJrb3Y6CiAgICAjIGZpbGwgdW51c2VkIHNwYWNlIHdpdGggYSB3YXRlciB0ZXh0dXJlCiAgICBvbmU6CiAgICAgICAgW0JCLi9CQkIvLkIuXSAtPiBbLi4uLy5JLi8uLi5dCiAgICAgICAgWy5JLi9JQkkvLkkuXSAtPiBbLi4uLy5JLi8uLi5dCgogICAgIyBkZWxldGUgd2F0ZXIgcGl4ZWxzIGF0IHJhbmRvbSwgZm9yIGFuIGFuaW1hdGVkIGVmZmVjdAogICAgb25lOiBbSV0gLT4gW0JdIGlmIEFOSU1BVEVfV0FURVIK,JavaScript,1,1,256,256,5): generates a weird landscape of cities, forests and lakes.

## Documentation

See the [documentation](doc/) and [implementation notes](notes/) for more information.
