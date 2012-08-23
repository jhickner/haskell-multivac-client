guard :shell, all_on_start: true do
  watch "client.hs" do |m|
    puts "\n\nCompiling..."
    `runghc #{m[0]}`
  end
end

# vim:ft=ruby
