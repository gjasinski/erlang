defmodule Lab5Mix do
  @moduledoc """
  Documentation for Lab5Mix.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Lab5Mix.hello
      :world

  """
  def read() do
    file =File.read!("pollution.csv")
    if length(file) < 5900 do
      "not ok"
    end
    for station <- String.split(file, "\n"), do: convert_line(station)
    IO.puts "hello world"
  end

  def convert_line(line) do
    [date, time, pos1, pos2, measure] = String.split(line, ",")
    [dd, mm, yyyy] = String.split(date, "-")
    [hh, min] = String.split(time, ":")
    %{:datetime => {{yyyy, mm, dd}, {hh, min, 0}}, :location => {pos1, pos2}, :pollutionLevel => measure}
  end

end
