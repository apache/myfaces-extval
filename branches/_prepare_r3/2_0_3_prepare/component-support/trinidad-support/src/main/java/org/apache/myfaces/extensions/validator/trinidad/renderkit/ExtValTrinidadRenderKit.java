/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.trinidad.renderkit;

import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKit;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.trinidadinternal.renderkit.RenderKitDecorator;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;

import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;
import javax.faces.render.ResponseStateManager;
import javax.faces.context.ResponseWriter;
import javax.faces.context.ResponseStream;
import java.io.Writer;
import java.io.OutputStream;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValTrinidadRenderKit extends RenderKitDecorator
{
    protected ExtValRenderKit extValRenderKit;

    public static final String ID = "EXTVAL_TRINIDAD_RENDERKIT";

    public ExtValTrinidadRenderKit(RenderKit wrapped)
    {
        this.extValRenderKit = new ExtValRenderKit(wrapped);
    }

    @Override
    public void addRenderer(String family, String rendererType, Renderer renderer)
    {
        this.extValRenderKit.addRenderer(family, rendererType, renderer);
    }

    @Override
    public Renderer getRenderer(String family, String rendererType)
    {
        return this.extValRenderKit.getRenderer(family, rendererType);
    }

    @Override
    public ResponseStateManager getResponseStateManager()
    {
        return this.extValRenderKit.getResponseStateManager();
    }

    @Override
    public ResponseWriter createResponseWriter(Writer writer, String s, String s1)
    {
        return this.extValRenderKit.createResponseWriter(writer, s, s1);
    }

    @Override
    public ResponseStream createResponseStream(OutputStream outputStream)
    {
        return this.extValRenderKit.createResponseStream(outputStream);
    }

    protected String getDecoratedRenderKitId()
    {
        return CoreRenderKit.BASE_RENDER_KIT_ID;
    }
}
