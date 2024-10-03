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
hideUnusedImplicitPins = True
addImplicitNotPortOnBrothers = False

class GateBranch:
    def __init__(self, map, gate, value=-1):
        self.map: BitsMap = map

        self.gate = gate
        self.iGate = gates.index(gate)
        self.value = value
        self.args: [GateBranch] = []

        # Optimization
        self.usage = 0
        self.children: [GateBranch] = []
        self.down_complexity = -1
        self.up_complexity = -1
        self.status_base = True # if contains only elementary pins (or not pins)
        self.max_port = 0 if gate != 'pin' else value
        self.involved_ports = {}
        self.num_involved_ports = -1

        if gate == 'pin':
            self.max_port = value
            self.involved_ports[value] = 1

        self.implicit_brothers = []

        # Cache
        self.last_hash = None

    def calc_complexity(self, complexity=0):
        # Calculate num involved ports
        self.num_involved_ports = 0
        for port, val in self.involved_ports.items():
            if val > 0:
                self.num_involved_ports += 1

        # Calculate complexity
        self.down_complexity = complexity

        complexity += 1
        max_complexity = 0
        for child in self.children:
            compl = child.calc_complexity(complexity)
            if compl > max_complexity:
                max_complexity = compl

        self.up_complexity = max_complexity
        return self.up_complexity + 1

    def increment_usage(self):
        self.usage += 1

    def get_port_index(self):
        if not self.status_base:
            return -1

        if self.gate == 'not':
            return self.args[0].value

        return self.value

    def get_port_not(self, port):
        if not self.status_base:
            return -1

        for arg in self.args:
            if arg.get_port_index() == port:
                return 1 if arg.gate == 'not' else 0

        return -1

    def add_implicit_port(self, index, update=True):
        notGate = GateBranch(self, 'not')
        notGate.add(self.map.pins[index])
        notGate = self.map.check_gate(notGate)
        self.add(notGate)

        if update:
            self.propagate_update()

        return notGate

    def add_implicit_port_upTo(self, upToIndex):
        addedPorts = []

        for i in range(self.max_port, upToIndex):
            notGate = self.add_implicit_port(i+1, False)
            addedPorts.append(notGate)

        self.propagate_update()

        return addedPorts

    def add_involved_ports(self, ports):
        for port, num in ports.items():
            if port not in self.involved_ports:
                self.involved_ports[port] = 0

            self.involved_ports[port] += num

    def remove_involved_ports(self, ports):
        for port, num in ports.items():
            self.involved_ports[port] -= num

    def remove(self, arg):
        if arg in self.args:
            self.args.remove(arg)

            if self in arg.children: # and not arg.status_base (ignored because useless)
                arg.children.remove(self)

            self.remove_involved_ports(arg.involved_ports)

    def add(self, arg, at=-1):

        if arg.status_base:
            if arg.max_port > self.max_port:
                self.max_port = arg.max_port

        self.add_involved_ports(arg.involved_ports)

        if arg.iGate > 1 or len(arg.args) > 0:
            self.status_base = False
            #self.implicit_brothers = None

        arg.children.append(self)

        if at < 0:
            self.args.append(arg)
        else:
            self.args.insert(at, arg)

        self.propagate_update()
        curHash = self.get_hash()

        if addImplicitNotPortOnBrothers: #todo: move in an apart function
            if self.status_base and self.gate == 'and':
                # Verify the validity of implicit brothers
                toRemove = []
                for bro in self.implicit_brothers:
                    if bro.status_base != self.status_base:
                        toRemove.append(bro)
                    else:
                        implicitPorts = bro.add_implicit_port_upTo(self.max_port)
                        if bro.get_hash() != curHash:
                            toRemove.append(bro)

                            if hideUnusedImplicitPins:
                                # Remove again forced implicit ports
                                for iport in implicitPorts:
                                    bro.remove(iport)

                for rem in toRemove:
                    self.implicit_brothers.remove(rem)

                # Look for implicit brothers
                if curHash in self.map.gates:
                    self.implicit_brothers.append(self.map.gates[curHash])

    def destroy(self, replace_with=None):
        for child in self.children:
            if replace_with is not None:
                if child not in replace_with.children:
                    pos = child.args.index(self)
                    child.add(replace_with, at=pos)

            child.args.remove(self)

        hash = self.get_hash()
        if hash in self.map.gates:
            del self.map.gates[hash]

    def optimize(self):
        for implicit_bro in self.implicit_brothers:
            implicit_bro.destroy(replace_with=self)

        if len(self.implicit_brothers) > 0:
            self.propagate_update()

        self.implicit_brothers.clear()

    def propagate_update(self):
        prevHash = None
        if self.last_hash is not None and self.last_hash in self.map.gates:
            prevHash = self.last_hash

        self.last_hash = None
        self.get_hash()

        if prevHash is not None:
            del self.map.gates[prevHash]
            self.map.gates[self.last_hash] = self

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


# Implement XOR only on final circuit (?)
# Remember: XOR = (A AND (NOT B)) OR ((NOT A) AND B)
# XOR(A, B, C) = (A & ~B & ~C) | (~A & B & ~C) | (~A & ~B & C) | (A & B & C) => XOR(XOR(A, B),C)

disableCheckGateOptimize = False
ignoreNotPin = True

class BitsMap:
    def __init__(self):
        self.pins: [GateBranch] = []
        self.gates = {}
        self.map = GateBranch(self, 'or')

    def final_compression(self):
        for pin in self.pins:
            pin.calc_complexity()

        print("check")

    def check_gate(self, gate) -> GateBranch:
        hash = gate.get_hash()
        if hash in self.gates:
            return self.gates[hash]
        else:
            self.gates[hash] = gate
            gate.optimize() if not disableCheckGateOptimize else None
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
                if ignoreNotPin:
                    continue

                notGate = GateBranch(self, 'not')
                notGate.add(pin)
                gate = self.check_gate(notGate)
                gate.increment_usage()

            andGate.add(gate)

        if len(andGate.args) == 0:
            return

        andGate = self.check_gate(andGate)
        andGate.increment_usage()

        if bit == 0:
            notAndGate = GateBranch(self, 'not')
            notAndGate.add(andGate)
            notAndGate = self.check_gate(notAndGate)
            notAndGate.increment_usage()
            self.map.add(notAndGate)
        else:
            self.map.add(andGate)


map = BitsMap()

if False:
    map.set([0], 1)
    map.set([1], 1)
    map.set([0, 1], 1)
    map.set([1, 1], 0)

if False:
    map.set([0], 1)
    map.set([1],0)
    map.set([0, 1], 0)
    map.set([1, 1], 1)

if True: # unneeded pin example
    map.set([0], 1)
    map.set([1], 0)
    map.set([0, 1], 1)
    map.set([1, 1], 0)

map.final_compression()

print("check")

# Esempio di utilizzo
'''
numero = 2
bit_list = get_bits(numero)
print(f"I bit di {numero} sono: {bit_list}")
'''
