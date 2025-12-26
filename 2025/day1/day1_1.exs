#!/usr/bin/env elixir
# Copyright 2025 John Hurst
# John Hurst (john.b.hurst@gmail.com)
# 2025-12-04
# See https://adventofcode.com/2025/day/1

offset = fn line ->
  line = String.trim(line)
  sgn = if String.first(line) == "R", do: 1, else: -1
  sgn * String.to_integer(String.slice(line, 1..-1//1))
end

mod100 = fn n -> rem(rem(n, 100) + 100, 100) end

newpos = fn current, offset -> mod100.(current + offset) end

[filename | _] = System.argv()

File.stream!(filename)
|> Stream.map(offset)
|> Enum.scan(50, newpos)
|> Enum.count(&(&1 == 0))
|> IO.inspect()
