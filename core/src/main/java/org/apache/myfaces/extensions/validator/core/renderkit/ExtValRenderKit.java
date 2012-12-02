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
package org.apache.myfaces.extensions.validator.core.renderkit;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.context.ResponseStream;
import javax.faces.context.ResponseWriter;
import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;
import javax.faces.render.ResponseStateManager;
import java.io.OutputStream;
import java.io.Writer;
import java.util.logging.Logger;

/**
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValRenderKit extends RenderKit
{
    protected final Logger logger = Logger.getLogger(getClass().getName());
    protected RenderKit wrapped;

    public ExtValRenderKit(RenderKit wrapped)
    {
        this.wrapped = wrapped;

        logger.fine(getClass().getName() + " instantiated");
    }

    public void addRenderer(String family, String rendererType, Renderer renderer)
    {
        if (renderer instanceof ExtValRendererWrapper)
        {
            wrapped.addRenderer(family, rendererType, renderer);
        }
        else
        {
            wrapped.addRenderer(family, rendererType, createWrapper(renderer));
        }
    }

    public Renderer getRenderer(String family, String rendererType)
    {
        Renderer renderer = wrapped.getRenderer(family, rendererType);

        if(renderer != null)
        {
            return renderer instanceof ExtValRendererWrapper ? renderer : createWrapper(renderer);
        }

        return renderer;
    }

    public ResponseStateManager getResponseStateManager()
    {
        return wrapped.getResponseStateManager();
    }

    public ResponseWriter createResponseWriter(Writer writer, String s, String s1)
    {
        return wrapped.createResponseWriter(writer, s, s1);
    }

    public ResponseStream createResponseStream(OutputStream outputStream)
    {
        return wrapped.createResponseStream(outputStream);
    }

    @UsageInformation(UsageCategory.REUSE)
    protected Renderer createWrapper(Renderer renderer)
    {
        return new ExtValRendererWrapper(renderer);
    }
}
