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
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;

import javax.faces.context.FacesContext;
import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.convert.ConverterException;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.logging.Logger;

/**
 * to support a custom proxy
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class ExtValLazyRendererProxy extends Renderer implements RendererProxy
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Renderer wrapped;

    public ExtValLazyRendererProxy(Renderer renderer)
    {
        this.wrapped = renderer;

        logger.finest("simple proxy created for " + renderer.getClass().getName());
    }

    @Override
    public void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        getLazyRenderer().decode(facesContext, uiComponent);
    }

    @Override
    public void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        getLazyRenderer().encodeBegin(facesContext, uiComponent);
    }

    @Override
    public void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        getLazyRenderer().encodeChildren(facesContext, uiComponent);
    }

    @Override
    public void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        getLazyRenderer().encodeEnd(facesContext, uiComponent);
    }

    @Override
    public String convertClientId(FacesContext facesContext, String s)
    {
        return getLazyRenderer().convertClientId(facesContext, s);
    }

    @Override
    public boolean getRendersChildren()
    {
        return getLazyRenderer().getRendersChildren();
    }

    public Object getCachedConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        if(getLazyRenderer() instanceof RendererProxy)
        {
            return ((RendererProxy)getLazyRenderer()).getCachedConvertedValue(facesContext, uiComponent, o);
        }
        return getLazyRenderer().getConvertedValue(facesContext, uiComponent, o);
    }

    @Override
    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        return getLazyRenderer().getConvertedValue(facesContext, uiComponent, o);
    }

    private Renderer getLazyRenderer()
    {
        Class<? extends Renderer> targetClass = ExtValCoreConfiguration.get().rendererProxy();

        if(targetClass != null)
        {
            Class[] argClasses = new Class[1];
            argClasses[0] = Renderer.class;

            try
            {
                Constructor constructor = targetClass.getConstructor(argClasses);
                return (Renderer)constructor.newInstance(this.wrapped);
            }
            catch (Exception e)
            {
                logger.warning("couldn't create: " + targetClass.getName());

                return this.wrapped;
            }
        }
        else
        {
            return this.wrapped;
        }
    }

    public Renderer getWrappedRenderer()
    {
        return this.wrapped;
    }
}