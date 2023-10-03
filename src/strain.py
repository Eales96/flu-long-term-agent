class Strain:
    def __init__(self, ag_x: float, ag_y: float):
        self.ag_x = ag_x
        self.ag_y = ag_y

    def __repr__(self):
        return f'{self.ag_x}, {self.ag_y}'

