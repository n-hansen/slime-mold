from slime import slime
import numpy as np
import math
import pygame
import time


class Simulation:
    def __init__(
        self,
        grid_size,
        agent_pct,
        decay=0.3,
        sensor_angle=math.radians(60),
        sensor_offset=5,
        rotation_angle=math.radians(22.5),
        step_size=1,
        deposit_amount=3,
        max_density=2,
    ):
        agent_count = math.ceil((grid_size * grid_size) * agent_pct)
        self.slime = slime()
        self.env = self.slime.init(
            decay,
            sensor_angle,
            sensor_offset,
            rotation_angle,
            step_size,
            deposit_amount,
            max_density,
            np.float32(np.random.random((grid_size, grid_size))),
            grid_size * np.float32(np.random.random((agent_count,))),
            grid_size * np.float32(np.random.random((agent_count,))),
            2 * math.pi * np.float32(np.random.random((agent_count,))),
        )

    def single_step(self):
        self.env = self.slime.simulation_step(self.env)

    def render_frame(self):
        return self.slime.render_frame(self.env).get()

    def agent_density(self):
        return self.slime.get_agent_density(self.env).get()


class SlimeQuit(Exception):
    pass


class GUI:
    def __init__(self, grid_size, render_diagnostics=True, dump_images=None):
        self.render_diagnostics = render_diagnostics
        self.grid_size = grid_size
        self.sim = Simulation(grid_size, 0.08)
        if dump_images:
            self.dump_images = [0, dump_images]
        else:
            self.dump_images = None

    def run(self):
        pygame.init()
        pygame.display.set_caption("Slime Mold Agent Simulation")
        size = (self.grid_size, self.grid_size)
        self.screen = pygame.display.set_mode(size)
        self.surface = pygame.Surface(size)
        self.font = pygame.font.Font(None, 26)

        try:
            self.loop()
        except SlimeQuit:
            return

    def loop(self):
        while True:
            self.render()
            self.handle_input()

    def render(self):
        start = time.time()
        frame = self.new_frame()
        end = time.time()
        diff_ms = (end - start) * 1000.0

        pygame.surfarray.blit_array(self.surface, frame)
        self.screen.blit(self.surface, (0, 0))
        self.show_text("Rendered in {:.2f} ms".format(diff_ms), (5, 5))
        pygame.display.flip()

        if self.dump_images:
            pygame.image.save(
                self.surface, self.dump_images[1].format(self.dump_images[0])
            )
            self.dump_images[0] += 1

    def new_frame(self):
        self.sim.single_step()
        frame = self.sim.render_frame()
        # ad = self.sim.agent_density()
        # frame = np.int32(np.clip(255 * tm, 0, 255)) << 16
        # frame = frame + np.int32(np.clip((25 * ad), 0, 255))
        return frame

    def show_text(self, what, where, color=(255, 0, 255), antialias=True):
        if self.render_diagnostics:
            text = self.font.render(what, antialias, color)
            self.screen.blit(text, where)

    def handle_input(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise SlimeQuit()
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_q:
                    raise SlimeQuit()


if __name__ == "__main__":
    g = GUI(1000, render_diagnostics=True,)
    g.run()
