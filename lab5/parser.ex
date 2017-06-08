defmodule parser do
  def read() do
    File.read!("pollution.csv") |> String.split(file, "\n")
    if length(file) < 5900 do
      "not ok"
    end

  end

  def convert_line(line) do
    [date, time, pos1, pos2, measure] = String.split(line, ",")
    [dd, mm, yyyy] = String.split(date, "-")
    [hh, mm] = String.split(time, ":")
    res = %{:datetime => {{yyyy, mm, dd}, {hh, mm, 0}}, :location => {pos1, pos2}, :pollutionLevel => measure}
  end

  def identifyStations(list_of_maps) do
    list |> Enum.reduce
  end
end
