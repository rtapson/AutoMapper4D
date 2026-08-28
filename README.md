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

- **Delphi.** VCL, Win32. The project files are in the 10.3 Rio format
  (`ProjectVersion` 18.8); the library is built and tested against Delphi 12.
  The demo application uses inline variable declarations, so it needs 10.3 or
  later.
- **[Spring4D](https://bitbucket.org/sglienke/spring4d).** Required, not
  optional — `AutoMapper.pas` uses `Spring.Collections` for `IDictionary` and
  `Spring.Reflection` for RTTI helpers.

Add Spring4D's `Source` directories to your project's unit search path, then add
`AutoMapper.pas` and `uFuzzyStringMatch.pas` to your project.

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

`AutoMapper.pas` also declares a class helper that puts the same two operations
on every object:

```pascal
Dto := Customer.Adapt<TCustomerDto>;   // returns a new object
Customer.Adapt<TCustomerDto>(Dto);     // maps onto an existing one
```

> **Note:** `Adapt` comes from `AutoMapper4D = class helper for TObject`. Delphi
> allows only one active helper per type in a given scope, so if your codebase
> has another `TObject` helper, whichever unit appears later in the `uses`
> clause wins and the other's methods become invisible — with no compiler
> warning. Use `TAutoMapper<T>.Map` directly if that is a concern.

### Explicit configuration

Pass a dictionary to control the mapping. The **key is the target property
name** and the **value is the source property name**:

```pascal
uses
  Spring.Collections, AutoMapper;

var
  Config : IDictionary<string, string>;
begin
  Config := TCollections.CreateDictionary<string, string>;
  Config.Add('FullName', 'Name');   // target FullName  <-  source Name

  Dto := TAutoMapper<TCustomerDto>.Map(Customer, Config);
```

> **Important:** supplying a configuration replaces automatic matching rather
> than extending it. Only the properties listed in the dictionary are mapped;
> everything else is left at its default. To override one property you must
> currently list them all.

An entry naming a source property that does not exist, or one whose value
cannot be assigned to the target, raises an exception rather than being
silently skipped.

## How properties are matched

For each writable property on the target, in order:

1. **Exact name.** Names are compared case-insensitively, so `CapTestProp`
   maps to `captestprop`.
2. **Fuzzy name.** If no name matches, every source property is scored with a
   Damerau-Levenshtein similarity ratio and the **highest scoring** candidate
   at or above **0.8** wins. This is what maps `First_Name` to `FirstName`
   (ratio 0.9).

A candidate is only considered if it is actually usable: the source property
must be readable, the target writable, and the types assignment compatible.
An unassignable top scorer does not block a lower scoring but valid match.

### What is not mapped

- **Object-typed properties are skipped.** There is no nested or deep mapping.
- Read-only target properties and write-only source properties are skipped.
- Type-incompatible pairs are skipped.
- Interface, record, and dynamic array properties are copied as-is, by
  reference or by value — no deep copy.

Nothing is reported when a target property finds no match; it simply keeps its
default value.

## Performance

Name matching and fuzzy scoring depend only on the source and target *types*,
so the resolved mapping is computed once per type pair and cached, then
replayed for each object. Mapping ~20,000 objects of an 8-property class runs
at roughly 2 µs per object.

The cache is thread-safe. Plans are built outside the lock, so a race on a cold
cache just means the same plan is built twice rather than a lock being held
across RTTI work.

Explicit configuration mappings are not cached, since the dictionary can differ
on every call.

## Building

`AutoMapperProjectGroup.groupproj` contains both projects:

- `AutoMapperTester.dproj` — a small VCL demo application
- `Tests/AutoMapperTests.dproj` — the DUnitX test suite

> The unit search paths stored in the `.dproj` files point at the Spring4D
> location on the original author's machine (`c:\Rio\Comps\Spring`,
> `C:\Code\spring4d1.2`). Adjust them to wherever Spring4D lives on yours.

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

## Lazarus / Free Pascal

The `fpcAutoMapperTests` and `.lpi` project files are a work in progress and
**do not currently build.** `AutoMapper.pas` depends on Spring4D and on dotted
RTL unit names, neither of which is available under FPC; `AutomaticConversion.log`
records the Lazarus conversion wizard aborting, and `automappertestcase1.pas` is
still the generated stub.

## License

[Apache License 2.0](LICENSE).
