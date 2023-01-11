from typing import List, Optional
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import opensmile
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import seaborn as sns
import torchaudio
from IPython import display
from IPython.lib.display import Audio
from ipywidgets import GridspecLayout, Output
from plotly.subplots import make_subplots
from plotly_resampler import FigureResampler
from speechbrain.pretrained.interfaces import VAD


# Load the VAD model
VAD_model = VAD.from_hparams(
    source="speechbrain/vad-crdnn-libriparty", savedir=".vad_model"
)


def analyze_utterance(
    utterance_path: Path,
    smile_lld: Optional[opensmile.Smile] = opensmile.Smile(
        feature_set=opensmile.FeatureSet.GeMAPSv01b,
        feature_level=opensmile.FeatureLevel.LowLevelDescriptors,
    ),
    feat_cols: List[str] = [
        "F0semitoneFrom27.5Hz_sma3nz",
        "jitterLocal_sma3nz",
        "shimmerLocaldB_sma3nz",
        "HNRdBACF_sma3nz",
    ],
    #
    # Listen to audio
    audio=True,
    audio_begin_s=10,
    audio_end_s=15,
    # Visualize audio (opensmile)
    plot_type="png",
    # Transform audio
    norm_audio=True,
    vad=True,
) -> None:
    """Analyze audio quality of a single File."""
    # ------------------ Load audio data ------------------ #
    ## The raw, 16 bit PCM wav path
    wav_path_orig = utterance_path
    arr_orig_wav_n, fs_orig = torchaudio.load(wav_path_orig, normalize=True)
    print("arr_orig_wav_n size", list(arr_orig_wav_n.size()), "orig fs", fs_orig)

    # resample the data if needed
    arr_16khz_n = torchaudio.functional.resample(
        arr_orig_wav_n, orig_freq=fs_orig, new_freq=16_000
    )
    # Save the 32 bit 16khz float audio data
    torchaudio.save("test.wav", arr_16khz_n, sample_rate=16_000)
    arr_orig_wav_n = arr_orig_wav_n.numpy().ravel()
    arr_16khz_n = arr_16khz_n.numpy().ravel()

    ## THe 16khz, 32 bit float wav path
    t_arr_n = np.arange(0, arr_16khz_n.shape[0]) / 16_000

    audio_list = []
    if audio:  # Listen to audio
        start, stop = audio_begin_s, audio_end_s
        print(f"Playing from {start} to {stop} seconds")
        audio_list.append(
            (
                "orig norm",
                arr_orig_wav_n[start * fs_orig : stop * fs_orig],
                fs_orig,
            )
        )
        if norm_audio:
            audio_list.append(
                (
                    "16khz norm",
                    arr_16khz_n[start * 16_000 : stop * 16_000],
                    16_000,
                )
            )

    if len(audio_list):
        grid = GridspecLayout(2, len(audio_list))
        for col_idx, (name, arr, fs) in enumerate(audio_list):
            out = Output()
            try:
                with out:
                    display.display(Audio(arr, rate=fs))
                grid[1, col_idx] = out
            except KeyError:
                continue
            out = Output()
            with out:
                display.display(name)
            grid[0, col_idx] = out

        display.display(grid)

    # Extract opensmile features
    if smile_lld is not None:
        df_smile_arr_orig_n = smile_lld.process_signal(arr_orig_wav_n, fs_orig)
        df_smile_wav = None
        df_smile_arr_n_16Khz = smile_lld.process_signal(arr_16khz_n, 16_000)

    e2e_boundaries_tuning = None
    if vad:
        e2e_boundaries_tuning = (
            VAD_model.upsample_boundaries(
                VAD_model.get_speech_segments(
                    audio_file="test.wav",
                    large_chunk_size=15,
                    small_chunk_size=1,
                    overlap_small_chunk=True,
                    apply_energy_VAD=True,
                    double_check=True,
                    # neural proba activation thresholds
                    activation_th=0.65,
                    deactivation_th=0.2,
                    # VAD energy activation thresholds
                    en_activation_th=0.6,
                    en_deactivation_th=0.3,
                ),
                audio_file="test.wav",
            )
            .numpy()
            .ravel()
        )

    # Plot
    n_rows = 1 + len(feat_cols)
    subplot_kwargs = {}
    if n_rows > 1:
        subplot_kwargs["vertical_spacing"] = 0.05

    fr = FigureResampler(
        make_subplots(
            rows=n_rows,
            shared_xaxes=True,
            **subplot_kwargs,
            specs=[[{"secondary_y": True}]] * n_rows,
        ),
        default_n_shown_samples=2500,
    )
    # Row 1: normalized wav + 16Khz resampled wav
    fr.add_trace(
        go.Scattergl(name="torch-norm"),  # "opacity": 0.5},
        hf_y=arr_16khz_n,
        hf_x=t_arr_n,
        max_n_samples=10_000 if plot_type in ["png", "return"] else 2500,
        col=1,
        row=1,
    )

    if e2e_boundaries_tuning is not None:
        fr.add_trace(
            go.Scattergl(name="VAD boundaries"),
            hf_y=e2e_boundaries_tuning,
            hf_x=t_arr_n,
            secondary_y=True,
            col=1,
            row=1,
        )

        # Add a rectangle where we would cut the audio
        where = np.where(e2e_boundaries_tuning > 0)[0]
        if not len(where):
            speech_start_idx, speech_end_idx = (
                0,
                len(e2e_boundaries_tuning) - 1,
            )
        else:
            speech_start_idx, speech_end_idx = where[0], where[-1]
        fr.add_vrect(
            x0=0,
            x1=max(0, t_arr_n[speech_start_idx] - 0.25),
            line_width=0,
            fillcolor="red",
            opacity=0.2,
        )
        fr.add_vrect(
            x0=min(t_arr_n[-1], t_arr_n[speech_end_idx] + 0.25),
            x1=t_arr_n[-1],
            line_width=0,
            fillcolor="red",
            opacity=0.2,
        )

    c_list = px.colors.qualitative.Plotly
    signal_names = []
    for i, feat_col in enumerate(feat_cols, start=2):
        fr.add_annotation(
            xref="x domain",
            yref="y domain",
            x=0.5,
            y=1.1,
            showarrow=False,
            text=f"<b>{feat_col}</b>",
            row=i,
            col=1,
        )

        if df_smile_wav is not None:
            fr.add_trace(
                {
                    "name": "Smile-orig-wav",
                    "line_color": c_list[2],
                    "showlegend": "opensmile-wav" not in signal_names,
                    "legendgroup": "opensmile-wav",
                },
                hf_y=df_smile_wav[feat_col],
                hf_x=df_smile_wav.reset_index().start.dt.total_seconds(),
                col=1,
                row=i,
            )
            signal_names.append("opensmile-wav")

        # Visualize the opensmile features
        for color_idx, (name, df) in enumerate(
            [
                ("Smile-orig-n", df_smile_arr_orig_n),
                ("Smile-16khz-n", df_smile_arr_n_16Khz),
            ],
            start=2,
        ):
            fr.add_trace(
                {
                    "name": name,  #  + '-' + feat_col,
                    "line_color": c_list[color_idx],
                    "showlegend": name not in signal_names,
                    "legendgroup": name,
                },
                hf_y=df[feat_col],
                hf_x=df.reset_index().start.dt.total_seconds(),
                col=1,
                row=i,
            )
            signal_names.append(name)

    # update layout and show
    fr.update_layout(
        height=200 + 150 * n_rows,
        title=str(utterance_path),
        title_x=0.5,
        template="plotly_white",
    )
    fr.update_xaxes(title_text="Time (s)")
    if plot_type == "png":
        fr.show(renderer="png", width=1400)
    elif plot_type == "dash":
        fr.show_dash(mode="inline", port=8044)
    elif plot_type == "return":
        return fr
    else:
        fr.show()
