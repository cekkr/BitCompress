import struct
from grouping import *
from itertools import combinations

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

# Generic functions
def gates_pins_to_indexes(pins):
    indexes = []
    for pin in pins:
        if pin.is_base_pin:
            indexes.append(pin.get_port())
    return indexes

def get_not_pins(pins):
    nots = []
    for pin in pins:
        if pin.gate == 'not' and pin.is_base_pin:
            nots.append(pin.args[0])
    return nots

def find_combinations (lista):
  """
  Questa funzione prende una lista di valori e restituisce tutte le possibili combinazioni di valori.

  Args:
    lista: Una lista di valori.

  Returns:
    Una lista di liste, dove ogni sottolista rappresenta una combinazione di valori.
  """
  combs = []
  for i in range(1, len(lista) + 1):
    for combination in combinations(lista, i):
      combs.append(list(combination))
  return combs

def by_size(list):
    by_size = {}
    for item in list:
        size = len(item)
        if size not in by_size:
            by_size[size] = []
        by_size[size].append(item)
    return by_size

def summation(num):
    res = 0
    for i in range(1, num+1):
        res += i
    return res

def join_ports(ports):
    join = ''
    for port in ports:
        if len(join) > 0:
            join += '.'
        join += str(port)
    return join

def strports_to_ports(str_ports):
    ports = str_ports.split('.')
    for i in range(0, len(ports)):
        ports[i] = int(ports[i])
    return ports

#########################################################

gates = ['pin', 'not', 'and', 'or', 'xor']
#enumerateGates = enumerate(gates)

debugHash = True
hideUnusedImplicitPins = True
addImplicitNotPortOnBrothers = False

class GateBranch:
    def __init__(self, map, gate, value=-1, is_basic = True):
        self.map: BitsMap = map

        # Type checking
        if type(value) is str:
            value = int(value)

        self.gate = gate
        self.i_gate = gates.index(gate)
        self.value = value
        self.args: [GateBranch] = []

        # Optimization
        self.usage = 0
        self.children: [GateBranch] = []
        self.down_complexity = -1
        self.up_complexity = -1
        self.is_base_pin = True if self.i_gate < 2 else False # if contains only elementary pins (or not pins)
        self.is_basic = is_basic # is basic table definition gate
        self.max_port = 0 if gate != 'pin' else value
        self.implicit = 1 if self.is_basic and self.gate == 'and' else 0 # 0 => no implicit, 1 => normal implicit (or not implicit if or)
        self.implicit_not = False #todo: Implement it (?)

        # 50 shades of ports
        self.ports = [] if gate != 'pin' else [value]
        self.involved_ports = {}
        self.num_involved_ports = -1

        if gate == 'pin':
            self.max_port = value
            self.involved_ports[value] = 1

        # Cache
        self.last_hash = None

    def calculate_args_in_ports(self, args_in_ports=None, distinguish_not=True):
        if args_in_ports is None:
            args_in_ports = {}

        for arg in self.args:
            if arg.is_base_pin and (distinguish_not or arg.gate is 'pin'):
                port = ('!' if arg.gate is 'not' else '') + str(arg.value)
                if port not in args_in_ports:
                    args_in_ports[port] = []
                args_in_ports[port].append(self)
            else:
                arg.calculate_args_in_ports(args_in_ports)

        return args_in_ports

    def get_gates_by_allports(self):
        gates = {}
        for arg in self.args:
            gates[arg.get_ports_hash()] = arg
        return gates

    def get_ports_hash(self):
        return join_ports(self.ports)

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

    def get_base_pins(self):
        #todo: cache into self.ports
        if self.is_base_pin:
            return [self]

        if self.gate == 'not': # if self.is_base_pin is by consequence a top pin
            return self.args[0].get_base_pins()

        base_pins = []
        for arg in self.args:
            if arg.is_base_pin:
                base_pins.append(arg)

        return base_pins

    def get_base_pin(self, pin_number):
        if self.is_base_pin and self.get_port() == pin_number:
            return self

        for arg in self.args:
            if arg.is_base_pin and arg.get_port() == pin_number:
                return arg

        return None

    def get_port(self):
        if not self.is_base_pin or self.i_gate > 1:
            return -1

        if self.gate == 'not':
            return self.args[0].value

        return self.value

    def get_port_not(self, port):
        if not self.is_base_pin:
            return -1

        for arg in self.args:
            if arg.get_port() == port:
                return 1 if arg.gate == 'not' else 0

        return -1

    def get_empty_gate(self):
        for arg in self.args:
            if arg.i_gate > 1 and len(arg.args) == 0:
                return arg
        return None

    def get_gate_with_ports(self, ports):
        ports = set(ports)
        for arg in self.args:
            if set(arg.ports) == ports:
                return arg

        return None

    def get_self_gates(self):
        selfGates = []
        for arg in self.args:
            if len(arg.args) == 1:
                selfGates.append(arg)
        return selfGates

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
                if num > 0 and port not in self.ports:
                    self.ports.append(port)

            self.involved_ports[port] += num

        self.ports.sort()

    def remove_involved_ports(self, ports):
        for port, num in ports.items():
            self.involved_ports[port] -= num

            if self.involved_ports[port] == 0:
                self.ports.remove(port)

    def set_always_true(self):
        print("OR always true")
        pass #todo: ok, this gate is always true

    def remove_ports(self, ports):
        # Remove the ports from the args (for port grouping or simplifications)
        toRemove = []
        for arg in self.args:
            if arg.is_base_pin and arg.get_port() in ports:
                toRemove.append(arg)
            else:
                arg.remove_ports(ports)

        toUpdate = len(toRemove) > 0
        for rem in toRemove:
            self.args.remove(rem)

        if toUpdate:
            self.propagate_update()

    def remove_duplicate_args(self):
        hashes = []
        to_remove = []

        for arg in self.args:
            arg_hash = arg.get_hash()
            if arg_hash in hashes:
                to_remove.append(arg)
            else:
                hashes.append(arg_hash)

        for rem in to_remove:
            self.remove(rem, as_duplicate=True)

    def group_by(self, ports, in_process=True):
        related_gates = []

        for gate in self.args:
            has_ports = True
            for port in gate.ports:
                if port not in ports:
                    has_ports = False
                    break

            if has_ports:
                related_gates.append(gate)

        if len(related_gates) > 0:
            group_gate = GateBranch(self.map, 'and')
            inner_gates = GateBranch(self.map, self.gate)

            for gate in related_gates:
                self.remove(gate)
                gate.remove_ports(ports)
                gate = self.map.check_gate(gate)
                inner_gates.add(gate)

            inner_gates = self.map.check_gate(inner_gates)

            for port in ports:
                port_pin = GateBranch(self.map, 'pin', port)
                port_pin = self.map.check_gate(port_pin)
                group_gate.add(port_pin)

            group_gate.add(inner_gates)
            group_gate = self.map.check_gate(group_gate)
            self.add(group_gate, in_process=in_process)


    def optimize_or(self):
        if self.gate != 'or':
            return

        # Snippet #1

        # Notes:
        # (A*B)+(A*!B) => AND!(A,B)+AND!(A) => AND!(AND(A))             AND!(A,B)+AND!(A) => B exclusion (DONE)
        # (A*B)+(!A*!B) => !XOR(A,B)                                    AND!(A,B)+AND!()
        # (!A*!B)+(!A*B)+(A*!B) => NOT!(AND(A,B))                       AND!()+AND!(A)+AND!(B) (DONE)
        # (!A*B)+(A*!B) => XOR(A,B)                                     OR!(AND!(B),AND!(A))
        # AND!()+AND!(B)+AND!(A) => NOT(AND(A,B)) (DONE)
        # NOT(A+B) => NOT(A)+NOT(B) (possible simplification)

        args_in_ports = self.calculate_args_in_ports()
        ports_groups = calculate_ports_groups(args_in_ports)

        if len(ports_groups) > 0:
            for ports in ports_groups:
                self.group_by(ports)

        args_in_ports = self.calculate_args_in_ports()
        empty_gate = self.get_empty_gate()

        # Look for NOT!(AND!(A, B)) => OR(NOT(A),NOT(B))
        # Or more generically AND!(A,B) as (A*B)+A+B
        #todo: Sorry, riki, but WHAT THE FUCK IS THAT?
        convertNotAndToOrNot = False
        if convertNotAndToOrNot:
            full_gates = [] # aims to have the gates that uses every port in the group

            connections = {}
            for port, args in args_in_ports.items():
                connections[port] = []
                for arg in self.args:
                    full_gates.append(arg)
                    for subport in arg.ports:
                        if subport not in connections:
                            connections[port].append(subport)

            group = list(connections.keys())
            for port, conn in connections.items():
                to_remove = []
                for gate in full_gates:
                    if port not in gate.ports:
                        to_remove.append(gate)

                for rem in to_remove:
                    full_gates.remove(rem)

                included = [port]
                for connPort in conn:
                    included.append(connPort)

                to_remove = []
                for groupPort in group:
                    if groupPort not in included:
                        to_remove.append(groupPort)

                for rem in to_remove:
                    group.remove(rem)

            if empty_gate is not None and len(full_gates) > 0: # it's always true
                return self.set_always_true()

            if len(group) > 0:
                self.remove_ports(group)

                andGate = GateBranch(self.map, 'and')

                for port in group:
                    andGate.add_port(port)

                if empty_gate is None: # is NOT AND
                    notGate = GateBranch(self.map, 'not')
                    notGate.add(andGate)
                    andGate = notGate

                andGate = self.map.check_gate(andGate)
                self.add(andGate, in_process=True)

        # Check for exclusion
        # (A*B)+(A) => !(A)
        # not for: (A*B)+(A*C) => A*(B+C)
        args_in_ports = self.calculate_args_in_ports()

        exclude = []
        for port, args in args_in_ports.items():
            common = {}
            sequences = []
            for arg in self.args:
                ports = arg.ports

                if port in ports:
                    ports.remove(port)

                comb = join_ports(ports)
                if comb in sequences:
                    exclude.append([port, common[comb], arg])
                else:
                    sequences.append(comb)
                    common[comb] = arg

        for excl in exclude:
            port, arg1, arg2 = excl
            main_arg = arg1 if port in arg1.ports else arg2
            self.remove(main_arg)

        # Look for XOR
        # (A*B)+(!A*!B) => !XOR(A,B) => AND!(A,B)+AND()
        # (!A*B)+(A*!B) => XOR(A,B) => AND!(A)+AND(B)
        # !XOR(A,B,C) => (!A*!B*!C)+(A*B*C)
        # (!A*B) + (A*!B) => !(!A*!B) + !(A*B) => !((A*B) + (!A*!B))
        # Simple and superficial implementation

        # Basic implementation: just check (A*B*..)+(!A*!B*!..) => !XOR
        if empty_gate is not None:
            args_in_ports = self.calculate_args_in_ports()
            ports = set(list(args_in_ports.keys()))

            full_gate = None
            for arg in self.args:
                if ports == set(arg.ports): # is arg.ports a secure way (for the moment)?
                    full_gate = arg
                    break

            if full_gate is not None and len(full_gate.ports) > 0:
                # There are opposites to simplificate in !XOR
                not_xor_gate = GateBranch(self.map, 'not')
                xor_gate = GateBranch(self.map, 'xor')
                for port in full_gate.ports:
                    pin = GateBranch(self.map, 'pin', port)
                    pin = self.map.check_gate(pin)
                    xor_gate.add(pin)

                xor_gate = self.map.check_gate(xor_gate)
                not_xor_gate.add(xor_gate)
                not_xor_gate = self.map.check_gate(not_xor_gate)

                self.remove(empty_gate)
                self.remove_ports(full_gate)
                self.add(not_xor_gate, in_process=True)

        # Advance implementation: XOR
        # (!A*B)+(A*!B) => XOR(A,B) => AND!(A)+AND(B)
        gates_by_allports = self.get_gates_by_allports()

        combinations = find_combinations(self.ports)

        combs_by_size = {}
        combs_inside = {}
        for comb in combinations:
            if len(comb) == 1:
                continue

            comb_hash = join_ports(comb)
            combs_in = []

            not_subs = False
            for comb_in in combinations:
                if len(comb_in) < len(comb):
                    is_comb_in = True
                    for p in comb_in:
                        if p not in comb:
                            is_comb_in = False
                            break

                    if is_comb_in:
                        comb_in_hash = join_ports(comb_in)
                        if comb_in_hash not in gates_by_allports:
                            not_subs = True
                            break
                        else:
                            combs_in.append(comb_in)

            if not not_subs and len(combs_in) > 0:
                combs_inside[comb_hash] = combs_in
                size = len(combs_in)
                if size not in combs_by_size:
                    combs_by_size[size] = []
                combs_by_size[size].append(comb_hash)

        xors = []
        excluded_combs = []
        for size, combs in combs_by_size.items():
            for comb in combs:
                ports = strports_to_ports(comb)

                sub_combs = find_combinations(ports)
                sub_combs_by_size = by_size(sub_combs)

                valid = True
                included = []
                for s in reversed(range(1, size)):
                    num_combs = size if s == 1 or s == size - 1 else summation(size-(s-1)) # check for its correctness

                    size_combs = []
                    for comb_in in sub_combs_by_size[s]:
                        comb_in_hash = join_ports(comb_in)

                        if comb_in_hash in excluded_combs:
                            continue

                        valid_comb_in = True
                        for comb_in_port in comb_in:
                            #todo: This would check if the combinations has all necessary sub-combinations,
                            # but in this way it makes no sense. Is this implementation necessary?
                            # If I would more clever or rested, I would know it without thinking about it too much.
                            if comb_in_port not in ports: # umma umma, this makes no sense. it's always true
                                valid_comb_in = False
                                break

                        if valid_comb_in:
                            size_combs.append(comb_in_hash)

                    if len(size_combs) == num_combs:
                        included.extend(size_combs)
                    else:
                        valid = False
                        break

                if valid:
                    xors.append(comb)
                    excluded_combs.extend(included)

        # Substitute obtained XORs
        for xor_ports in xors:
            xor_ports = strports_to_ports(xor_ports)
            xor_gate = GateBranch(self.map, 'xor')

            for port in xor_ports:
                pin = GateBranch(self.map, 'pin', port)
                pin = self.map.check_gate(pin)
                xor_gate.add(pin)

            xor_gate = self.map.check_gate(xor_gate)
            self.add(xor_gate, in_process=True)

            # Remove ports combination
            combs = find_combinations(xor_ports)
            for comb in combs:
                if len(comb) < len(xor_ports):
                    comb_gate = self.get_gate_with_ports(comb)
                    if comb_gate is not None:
                        self.remove(comb_gate)
                    else:
                        print("PAY ATTENTION: gate ports combination doesn't exist")


        print("check")

    def remove(self, arg, as_duplicate=False):
        if arg in self.args:
            self.args.remove(arg)

            if not as_duplicate:
                if self in arg.children:
                    arg.children.remove(self)

                self.remove_involved_ports(arg.involved_ports)
                arg.check_if_used()

    # todo: superfluous function
    def add_port(self, port):
        pin = GateBranch(self.map, 'pin', int(port))
        pin = self.map.check_gate(pin)
        self.add(pin)

    def add(self, arg, at=-1, in_process=False):

        # Prevent duplicate
        arg_hash = arg.get_hash()
        for a in self.args:
            if arg_hash == a.get_hash():
                return

        if arg.is_base_pin:
            if arg.max_port > self.max_port:
                self.max_port = arg.max_port

        self.add_involved_ports(arg.involved_ports)

        if self.i_gate > 1 or (self.gate == 'not' and not arg.is_base_pin):
            self.is_base_pin = False

        arg.children.append(self)

        if at < 0:
            self.args.append(arg)
        else:
            self.args.insert(at, arg)

        if not in_process: # remove redundancies in OR
            self.optimize_or()

        self.propagate_update()

    def destroy(self, replace_with=None):
        for child in self.children:
            if replace_with is not None:
                if child not in replace_with.children:
                    pos = child.args.index(self)
                    child.add(replace_with, at=pos)

            child.args.remove(self)

        self.check_if_used()

    def check_if_used(self):
        if len(self.children) == 0:
            hash = self.get_hash()
            if hash in self.map.gates:
                del self.map.gates[hash]

    def propagate_update(self):
        prevHash = None
        if self.last_hash is not None and self.last_hash in self.map.gates:
            prevHash = self.last_hash

        self.last_hash = None
        _hash = self.get_hash()

        if prevHash is not None:
            del self.map.gates[prevHash]
            self.map.gates[_hash] = self

        for child in self.children:
            child.propagate_update()

        return _hash

    def __repr__(self):
        return self.get_hash()

    def get_hash(self) -> str:
        if self.last_hash is not None:
            return self.last_hash

        hash = ''

        hash += str(self.i_gate)

        if self.implicit:
            if self.implicit_not:
                hash += '!'
            else:
                hash += '&'

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
ignoreNotBit = True

class BitsMap:
    def __init__(self):
        self.pins: [GateBranch] = []
        self.gates = {}
        self.map = GateBranch(self, 'or')

    def final_compression(self):
        for pin in self.pins:
            pin.calc_complexity()

    def check_gate(self, gate) -> GateBranch:
        hash = gate.get_hash()
        if hash in self.gates:
            return self.gates[hash]
        else:
            self.gates[hash] = gate
            #gate.optimize() if not disableCheckGateOptimize else None
            return gate

    def set(self, series, bit):
        if ignoreNotBit and bit == 0:
            return

        andGate = GateBranch(self, 'and')

        andGate.implicit = True
        andGate.implicit_not = True if bit == 1 else False

        for i in range(0, len(series)):
            if len(self.pins) <= i:
                pin = GateBranch(self, 'pin', i)
                self.pins.append(pin)

            pin = self.pins[i]
            pin.increment_usage()

            gate = pin

            if series[i] == 0: # != bit
                if ignoreNotPin:
                    continue

                notGate = GateBranch(self, 'not')
                notGate.add(pin)
                gate = self.check_gate(notGate)
                gate.increment_usage()

            andGate.add(gate)

        andGate = self.check_gate(andGate)
        andGate.increment_usage()

        self.map.add(andGate)

map = BitsMap()

if True:
    map.set([0], 1)
    map.set([1], 1)
    map.set([0, 1], 1)
    map.set([1, 1], 0)

if False:
    map.set([0], 1)
    map.set([1],0)
    map.set([0, 1], 0)
    map.set([1, 1], 1)

if False: # unneeded pin example
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
