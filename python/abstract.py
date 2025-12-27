'''
abc = abstract base class 
demo
'''
from abc import ABC, abstractmethod

class animal(ABC):
    def __init__(self):
        self.name = ''
    @abstractmethod
    def call(self):
        pass

class dog(animal):
    def __init__(self):
        self.name = 'dog'
    def call(self):
        print('woof')

class cat(animal):
    def __init__(self):
        self.name = 'cat'
    def call(self):
        print('meow')

class iguana(animal):
    def __init__(self):
        self.name = 'iguana'
    def call(self):
        print('iiiiii')

b = dog()
c = cat()
d = iguana()

b.call()
c.call()
d.call()

print(d.name)
