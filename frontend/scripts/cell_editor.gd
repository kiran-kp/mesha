extends TextEdit

var view: Container = null
var index: int = 0
const cols: int = 3

# Called when the node enters the scene tree for the first time.
func _ready():
	self.view = $"../MainView"
	self.grab_focus()
	pass

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(_delta):
	pass

func _on_text_changed():
	var c = self.get_caret_column()
	if (c == 0):
		var t = self.text.split("\n", false)
		if (t.size() > 0):
			var result = MeshaServer.eval(t[t.size() - 1])
			var m = MeshaCell.new()
			self.view.add_child(m)
			const cell_size := 100
			# m.set_global_position(Vector2(cell_size * (self.index % self.cols), cell_size *(self.index / self.cols)))
			m.set_text(result)
			# m.size = Vector2(cell_size, cell_size)
			self.insert_text_at_caret(result + "\n")
