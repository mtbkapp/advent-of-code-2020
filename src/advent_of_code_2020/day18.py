class TokenStream:
    def __init__(self, line):
        self.tokens = line.replace(" ", "")
        self.idx = 0

    def take(self):
        if (self.idx < len(self.tokens)):
            token = self.tokens[self.idx]
            self.idx += 1
            return token
        else:
            return -1



    

line = '(3 * 5) * 5 + (2 + (8 + 4) + (8 * 5 + 3))'
ts = TokenStream(line)

