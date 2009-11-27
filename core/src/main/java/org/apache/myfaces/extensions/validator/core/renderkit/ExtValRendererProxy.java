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
import org.apache.myfaces.extensions.validator.core.storage.RendererProxyStorageEntry;
import org.apache.myfaces.extensions.validator.core.storage.RendererProxyStorage;
import org.apache.myfaces.extensions.validator.core.ProjectStage;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import javax.faces.application.FacesMessage;
import java.io.IOException;

/**
 * to avoid multiple calls of renderer methods within renderer interceptors (e.g. for encode, decode,...)
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValRendererProxy extends Renderer
{
    public static final String KEY = ExtValRendererProxy.class.getName() + ":KEY";
    protected final Log logger = LogFactory.getLog(getClass());
    
    protected Renderer wrapped;

    public ExtValRendererProxy(Renderer renderer)
    {
        this.wrapped = renderer;

        if(logger.isTraceEnabled())
        {
            logger.trace("proxy created for " + renderer.getClass().getName());
        }
    }

    @Override
    public void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        RendererProxyStorageEntry entry = getRendererEntry(facesContext, uiComponent);

        if (!entry.isDecodeCalled())
        {
            entry.setDecodeCalled(true);

            try
            {
                this.wrapped.decode(facesContext, uiComponent);
            }
            catch (RuntimeException r)
            {
                resetComponentProxyMapping();
                throw r;
            }
        }
        else
        {
            tryToCreateMessage("decode");
        }
    }

    @Override
    public void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        RendererProxyStorageEntry entry = getRendererEntry(facesContext, uiComponent);

        if (!entry.isEncodeBeginCalled())
        {
            entry.setEncodeBeginCalled(true);
            try
            {
                this.wrapped.encodeBegin(facesContext, uiComponent);
            }
            catch (IOException e)
            {
                resetComponentProxyMapping();
                throw e;
            }
            catch (RuntimeException r)
            {
                resetComponentProxyMapping();
                throw r;
            }
        }
        else
        {
            tryToCreateMessage("encodeBegin");
        }
    }

    @Override
    public void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        RendererProxyStorageEntry entry = getRendererEntry(facesContext, uiComponent);

        if (!entry.isEncodeChildrenCalled())
        {
            entry.setEncodeChildrenCalled(true);

            try
            {
                this.wrapped.encodeChildren(facesContext, uiComponent);
            }
            catch (IOException e)
            {
                resetComponentProxyMapping();
                throw e;
            }
            catch (RuntimeException r)
            {
                resetComponentProxyMapping();
                throw r;
            }
        }
        else
        {
            tryToCreateMessage("encodeChildren");
        }
    }

    @Override
    public void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        RendererProxyStorageEntry entry = getRendererEntry(facesContext, uiComponent);

        if (!entry.isEncodeEndCalled())
        {
            entry.setEncodeEndCalled(true);

            try
            {
                this.wrapped.encodeEnd(facesContext, uiComponent);
            }
            catch (IOException e)
            {
                resetComponentProxyMapping();
                throw e;
            }
            catch (RuntimeException r)
            {
                resetComponentProxyMapping();
                throw r;
            }
        }
        else
        {
            tryToCreateMessage("encodeEnd");
        }
    }

    @Override
    public String convertClientId(FacesContext facesContext, String s)
    {
        try
        {
            return wrapped.convertClientId(facesContext, s);
        }
        catch (RuntimeException r)
        {
            resetComponentProxyMapping();
            throw r;
        }
    }

    @Override
    public boolean getRendersChildren()
    {
        try
        {
            return wrapped.getRendersChildren();
        }
        catch (RuntimeException t)
        {
            resetComponentProxyMapping();
            throw t;
        }
    }

    @Override
    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        RendererProxyStorageEntry entry = getRendererEntry(facesContext, uiComponent);

        if (entry.getConvertedValue() == null)
        {
            try
            {
                entry.setConvertedValue(wrapped.getConvertedValue(facesContext, uiComponent, o));
            }
            catch (RuntimeException r)
            {
                resetComponentProxyMapping();
                throw r;
            }
        }
        else
        {
            tryToCreateMessage("getConvertedValue");
        }
        return entry.getConvertedValue();
    }

    protected RendererProxyStorageEntry getRendererEntry(FacesContext facesContext, UIComponent uiComponent)
    {
        String key = uiComponent.getClientId(facesContext);

        key += getOptionalKey(facesContext, uiComponent);

        if (!getRendererStorage().containsEntry(getRendererKey(), key))
        {
            getRendererStorage().setEntry(getRendererKey(), key, new RendererProxyStorageEntry());
        }
        return getRendererStorage().getEntry(getRendererKey(), key);
    }

    protected String getOptionalKey(FacesContext facesContext, UIComponent uiComponent)
    {
        return "";
    }

    protected String getRendererKey()
    {
        return this.wrapped.getClass().getName();
    }

    private RendererProxyStorage getRendererStorage()
    {
        return ExtValUtils.getStorage(RendererProxyStorage.class, RendererProxyStorage.class.getName());
    }

    private void resetComponentProxyMapping()
    {
        //reset component proxy mapping
        ExtValUtils.resetStorage(RendererProxyStorage.class, RendererProxyStorage.class.getName());
    }

    private void tryToCreateMessage(String methodName)
    {
        if(ProjectStage.is(ProjectStage.Development))
        {
            String message = "double call of " + this.wrapped.getClass().getName() + "#" + methodName + " filtered. " +
                    "this optimization might lead to incompatibilities with some component libs. " +
                    "in such a case use the support module for the component lib or use: " +
                    "ExtValContext.getContext().addGlobalProperty(ExtValRendererProxy.KEY, null); " +
                    "in a startup listener";

            FacesContext.getCurrentInstance()
                    .addMessage(null, ExtValUtils.createFacesMessage(FacesMessage.SEVERITY_WARN, message, message));

            if(logger.isWarnEnabled())
            {
                logger.warn(message);
            }
        }

        if(logger.isDebugEnabled())
        {
            logger.debug("turn on the development mode for further information, if something is displayed wrong.");
        }
    }
}
