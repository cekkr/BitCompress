import struct


def get_bits(n):
    """
  Restituisce una lista con i bit di un intero dato in input.

  Args:
    n: L'intero di input.

  Returns:
    Una lista con i bit dell'intero, dal bit meno significativo al bit più significativo.
  """
    bits = []
    while n > 0:
        bits.append(n % 2)
        n //= 2
    return bits  # Inverte la lista per avere l'ordine corretto


def float_to_bits(f, size=32):
    """Converte un numero float in una stringa di bit.

  Args:
    f: Il numero float da convertire.
    size: La dimensione del float in bit (32 o 64). Default è 32.

  Returns:
    Una stringa che rappresenta il numero float in binario.
  """

    if size == 32:
        fmt = 'f'
    elif size == 64:
        fmt = 'd'
    else:
        raise ValueError("Size deve essere 32 o 64")

    # Impacchetta il float come stringa di byte
    byte_string = struct.pack(fmt, f)

    # Converti la stringa di byte in una sequenza di bit
    bits = ''.join(f'{byte:08b}' for byte in byte_string)
    return bits


# print(float_to_bits(10.5, size=64))

#########################################################
#########################################################
#########################################################

gates = ['pin', 'not', 'and', 'or']
#enumerateGates = enumerate(gates)

debugHash = True


class GateBranch:
    def __init__(self, map, gate, value=-1):
        self.map = map

        self.gate = gate
        self.iGate = gates.index(gate)
        self.value = value
        self.args: [GateBranch] = []

        # Optimization
        self.usage = 0
        self.children: [GateBranch] = []
        self.down_complexity = 0
        self.up_complexity = 0

        self.implicit_brothers = []

        # Cache
        self.last_hash = None

    def increment_usage(self):
        self.usage += 1

    def add(self, arg):
        arg.children.append(self)
        self.args.append(arg)

        self.propagate_update()

        # Look for implicit brothers
        curHash = self.get_hash()
        if curHash in self.map.gates:
            self.implicit_brothers.append(self.map.gates[curHash])

        # Calculate complexities
        comp = self.up_complexity + 1
        if comp > arg.up_complexity:
            arg.up_complexity = comp

        comp = arg.down_complexity + 1
        if comp > self.down_complexity:
            self.down_complexity = comp

    def destroy(self, replace_with=None):
        for child in self.children:
            if replace_with is not None:
                if child not in replace_with.children:
                    pos = child.args.index(self)
                    child.args.insert(pos, replace_with)
                else:
                    child.args.append(replace_with)

            child.args.remove(self)

    def optimize(self):
        for implicit_bro in self.implicit_brothers:
            implicit_bro.destroy(replace_with=self)

        self.implicit_brothers.clear()

    def propagate_update(self):
      self.last_hash = None
      for child in self.children:
        child.propagate_update()

    def __repr__(self):
        return self.get_hash()

    def get_hash(self) -> str:
        if self.last_hash is not None:
            return self.last_hash

        hash = str(self.iGate)

        if self.value >= 0:
            hash += ':' + str(self.value)

        if len(self.args) > 0:
            if debugHash:
                hash += "("

            first = debugHash
            for arg in self.args:
                if first:
                    first = False
                else:
                    hash += ','

                hash += arg.get_hash()

            if debugHash:
                hash += ")"

        self.last_hash = hash
        return hash


# Remember: XOR = (A AND (NOT B)) OR ((NOT A) AND B)
optimizeNotNot = False

class BitsMap:
    def __init__(self):
        self.pins: [GateBranch] = []
        self.gates = {}
        self.map = GateBranch(self, 'or')

    def check_gate(self, gate) -> GateBranch:
        hash = gate.get_hash()
        if hash in self.gates:
            return self.gates[hash]
        else:
            self.gates[hash] = gate
            gate.optimize()
            return gate

    def set(self, series, bit):
        andGate = GateBranch(self, 'and')

        for i in range(0, len(series)):
            if len(self.pins) <= i:
                pin = GateBranch(self, 'pin', i)
                self.pins.append(pin)

            pin = self.pins[i]
            pin.increment_usage()

            gate = pin

            if series[i] == 0:
                notGate = GateBranch(self, 'not')
                notGate.add(pin)
                gate = self.check_gate(notGate)
                gate.increment_usage()

            andGate.add(gate)

        if len(andGate.args) > 1 or not optimizeNotNot:
            andGate = self.check_gate(andGate)
            andGate.increment_usage()
        else:
            andGate = andGate.args[0]

        if bit == 0:
            if andGate.gate == 'not' and optimizeNotNot:
                self.map.add(andGate.args[0])
            else:
                notAndGate = GateBranch(self, 'not')
                notAndGate.add(andGate)
                notAndGate = self.check_gate(notAndGate)
                notAndGate.increment_usage()
                self.map.add(notAndGate)
        else:
            self.map.add(andGate)


map = BitsMap()
map.set([0], 1)
map.set([1], 1)
map.set([0, 1], 1)
map.set([1, 1], 0)

print("check")

# Esempio di utilizzo
'''
numero = 2
bit_list = get_bits(numero)
print(f"I bit di {numero} sono: {bit_list}")
'''
