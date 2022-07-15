import "./style.sass";
import { main } from "../output/Main";
var unsub = main();

if (module.hot) {
	module.hot.accept("../output/Main", function () {
		unsub();
		unsub = main();
	});
}
