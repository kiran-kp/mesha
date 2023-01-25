extends TextEdit

# Called when the node enters the scene tree for the first time.
func _ready():
	pass

# Called every frame. 'delta' is the elapsed time since the previous frame.
func _process(delta):
	pass

func _on_text_changed():
	var c = self.get_caret_column()
	if (c == 0):
		var t = self.text.split("\n", false)
		self.insert_text_at_caret(LispServer.eval(t[t.size() - 1]) + "\n")
