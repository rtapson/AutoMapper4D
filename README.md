# AutoMapper4D

A small Delphi library that copies property values between unrelated classes
using RTTI, so entity objects can be mapped to domain or DTO objects without
hand-written assignment code.

Properties are matched by name. Where the names differ slightly — `First_Name`
against `FirstName`, say — a fuzzy match fills the gap, and an explicit
configuration dictionary handles the cases fuzzy matching should not guess at.

```pascal
uses
  AutoMapper;

var
  Dto : TCustomerDto;
begin
  Dto := TAutoMapper<TCustomerDto>.Map(Customer);
  try
    // ...
  finally
    Dto.Free;
  end;
end;
```

## Requirements

- **Delphi.** VCL, Win32. Built and tested against Delphi 12. The project files
  are in the Delphi 12 format (`ProjectVersion` 20.4), so an older IDE will not
  open them directly — the units themselves are less demanding, though the demo
  application uses inline variable declarations and so needs 10.3 or later.
- **Free Pascal 3.2.2** also works — see [Free Pascal / Lazarus](#free-pascal--lazarus)
  for the two limitations that come with it.
- **No third-party dependencies.** The library uses only the RTL —
  `System.Rtti`, `System.TypInfo` and `System.Generics.Collections` (undotted
  under FPC).

Add `AutoMapper.pas` and `uFuzzyStringMatch.pas` to your project. That is all -
plus `AutoMapper.Helper.pas` if you want the `Adapt<T>` sugar.

## Usage

### Map to a new object

`Map` constructs the target and returns it. **The caller owns the result** and
is responsible for freeing it.

```pascal
Dto := TAutoMapper<TCustomerDto>.Map(Customer);
```

### Map onto an existing object

```pascal
TAutoMapper<TCustomerDto>.Map(Customer, Dto);
```

Only properties that can be matched are written; everything else on the target
is left untouched.

### The `Adapt<T>` helper

An optional class helper puts the same two operations on every object:

```pascal
uses
  AutoMapper, AutoMapper.Helper;   // the helper lives in its own unit

Dto := Customer.Adapt<TCustomerDto>;   // returns a new object
Customer.Adapt<TCustomerDto>(Dto);     // maps onto an existing one
```

> **Why a separate unit:** `Adapt` comes from `AutoMapper4D = class helper for
> TObject`, and Delphi allows only **one** active helper per type in a given
> scope — whichever unit appears later in the `uses` clause wins, and the
> other's methods silently become invisible with no compiler warning. Keeping
> it out of `AutoMapper.pas` means using the mapper does not cost you your one
> `TObject` helper slot. Add `AutoMapper.Helper` only if you want the sugar.

### Explicit configuration

Pass a dictionary to control the mapping. The **key is the target property
name** and the **value is the source property name**:

```pascal
uses
  System.Generics.Collections, AutoMapper;

var
  Config : TDictionary<string, string>;
begin
  Config := TDictionary<string, string>.Create;
  try
    Config.Add('FullName', 'Name');   // target FullName  <-  source Name
    Dto := TAutoMapper<TCustomerDto>.Map(Customer, Config);
  finally
    Config.Free;
  end;
```

The mapper never takes ownership of the dictionary — create and free it
yourself.

A configuration is an **override layer, not a whitelist**. Properties it names
are redirected; every other target property is still matched automatically by
name and fuzzily. So you only list the awkward cases:

```pascal
//   Dto.FullName  <-  Customer.Name    (from the configuration)
//   Dto.Age       <-  Customer.Age     (matched automatically)
//   Dto.Email     <-  Customer.Email   (matched automatically)
```

An entry naming a source property that does not exist, or one whose value
cannot be assigned to the target, raises `EAutoMapperError` rather than being
silently skipped. Mapping from or onto `nil` raises `EAutoMapperError` too.
## How properties are matched

For each mappable property on the target, in order:

1. **Exact name.** Names are compared case-insensitively, so `CapTestProp`
   maps to `captestprop`.
2. **Fuzzy name.** If no usable name matches, every source property is scored
   with a Damerau-Levenshtein similarity ratio and the **highest scoring**
   candidate at or above the threshold (**0.8** by default) wins. This is what
   maps `First_Name` to `FirstName` (ratio 0.9).

A candidate is only considered if it is actually usable: the source property
must be readable, the target writable, and the types assignment compatible. An
unassignable top scorer does not block a lower scoring but valid match, and an
exact name match that turns out to be unusable falls through to fuzzy matching
rather than counting as matched.

### Nested objects

When **both** sides are object properties, the mapper descends into the
instance the target **already holds**:

```pascal
Dto := TCustomerDto.Create;   // Dto.Address already constructed
TAutoMapper<TCustomerDto>.Map(Customer, Dto);
//   Dto.Address is mapped into — the same instance, never replaced
```

Nothing is ever constructed for you, so ownership never changes hands: if the
target's object property is `nil` it is simply skipped. Recursion is capped at
16 levels, after which `EAutoMapperError` is raised, since two object graphs
that both contain a cycle would otherwise recurse forever.

### What is not mapped

- Read-only target properties and write-only source properties are skipped.
- Type-incompatible pairs are skipped.
- A nested object property whose **target** side is `nil` is skipped.
- Interface, record, and dynamic array properties are copied as-is, by
  reference or by value — no deep copy.

By default an unmatched target property simply keeps its value. Turn on strict
mode to make that an error instead — see below.

## Settings

Both are global, and live on `TMapperEngine`.

```pascal
//Reject anything below 0.95 as a fuzzy match. Changing this discards
//cached plans, since they were resolved against the old threshold.
TMapperEngine.FuzzyMatchThreshold := 0.95;

//Raise EAutoMapperError when a target property matches nothing, instead of
//quietly leaving it at its default. Off by default.
TMapperEngine.Strict := True;

//Discard cached plans, e.g. after unloading a package.
TMapperEngine.ClearCache;
```

Strict mode counts an explicit configuration entry as a match, and runs its
check **before** anything is written — so a strict failure leaves the target
exactly as it was.
## Performance

Name matching and fuzzy scoring depend only on the source and target *types*,
so the resolved mapping is computed once per type pair and cached, then
replayed for each object. Property type handles are resolved into the plan as
well, so replaying it never re-enters the RTTI pool. Mapping ~20,000 objects of
an 8-property class runs at roughly 2 µs per object.

The cache is thread-safe, and was stress-tested with eight threads mapping
concurrently from a cold cache. Explicit configuration mappings are not cached,
since the dictionary can differ on every call.

## Building

`AutoMapperProjectGroup.groupproj` contains both projects:

- `AutoMapperTester.dproj` — a small VCL demo application
- `Tests/AutoMapperTests.dproj` — the DUnitX test suite

### Resources

`AutoMapperTester.res` is a compiled binary and is not committed;
`AutoMapperTester.rc` is the source it is built from. The IDE and MSBuild both
regenerate a missing project `.res` automatically, so no extra step is needed
there. A direct `dcc32` build does not, and fails with:

```
Error: E1026 File not found: 'AutoMapperTester.res'
```

Run the build step first in that case:

```
build-res.cmd
```

It uses Microsoft's `rc.exe`, which ships with Delphi. The legacy `brcc32`
cannot compile the numeric `RT_MANIFEST` entry this script uses.

## Tests

`Tests/AutoMapperTests.dproj` is a DUnitX suite covering string, integer,
`TDateTime`, boolean, enum, case-mismatched and fuzzy-matched properties. It
runs as a console application, and supports TestInsight when built with the
`TESTINSIGHT` define.

For Free Pascal there is a separate console test at `Tests/fpc`; see
[Free Pascal / Lazarus](#free-pascal--lazarus).

## Free Pascal / Lazarus

Supported, and verified against **FPC 3.2.2**. The same sources build on both
compilers; the `uses` clauses switch on `{$IFDEF FPC}`.

```
Tests\fpc\build-and-run.cmd
```

builds and runs `Tests/fpc/FpcAutoMapperTests.lpr`, a console program covering
automatic mapping, case-insensitive and fuzzy name matching, mapping onto an
existing object, both `Adapt<T>` overloads, configuration, and the cached plan
path.

### Two limitations on FPC

Both come from Free Pascal's RTTI rather than from this library, and both are a
strict subset of the Delphi behaviour — nothing maps *differently*, only fewer
things map.

**Only `published` properties are visible.** Delphi's extended RTTI covers
public members by default; FPC's `Rtti` is built on classic published-only
RTTI, so a class with `public` properties presents *zero* properties to the
mapper. Declare mapped properties `published`, with `{$M+}` if the class does
not already descend from `TPersistent`. `published` works identically on
Delphi, so one set of classes serves both — `uTestClassA`/`uTestClassB` show
the pattern.

**Fewer types map.** FPC's `TRttiProperty.GetValue`/`SetValue` implement a
subset of type kinds and *raise* on the rest, so unsupported kinds are skipped.
Most visibly, **set properties are not mapped on FPC** (`TTests` in the
samples). There is also no `TValue.TryCast` in FPC, so the FPC build maps
identically typed properties only, where Delphi also converts within the
ordinal/float and string families.

`automappertestcase1.pas` in the repository root is still the generated fpcunit
stub, whose only test calls `Fail('Write your own test')`. The program above is
the working test.

## License

[Apache License 2.0](LICENSE).
