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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.UsageEnum;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ValidationUtils;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import java.io.IOException;
import java.lang.reflect.Method;

/**
 * Default approach to avoid proxies for converters and the adapter fallback.
 * It requires that components delegate getConvertedValue to a renderer.
 * If it isn't the case for your component lib use:
 * org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory
 * and
 * org.apache.myfaces.extensions.validator.core.proxy.ProxyMappingPhaseListener
 * <p/>
 * This wrapper will also implement client-side validation behaviour
 *
 * @author Gerhard Petracek
 */
@UsageInformation(UsageEnum.INTERNAL)
public class ExtValRendererWrapper extends Renderer
{
    private static Boolean isAlternativeAvailable = null;
    private Renderer wrapped;

    public ExtValRendererWrapper(Renderer wrapped)
    {
        this.wrapped = wrapped;
    }

    public void decode(FacesContext facesContext, UIComponent uiComponent)
    {
        wrapped.decode(facesContext, uiComponent);
    }

    public void encodeBegin(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        wrapped.encodeBegin(facesContext, uiComponent);
    }

    public void encodeChildren(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        wrapped.encodeChildren(facesContext, uiComponent);
    }

    public void encodeEnd(FacesContext facesContext, UIComponent uiComponent)
        throws IOException
    {
        wrapped.encodeEnd(facesContext, uiComponent);
    }

    public String convertClientId(FacesContext facesContext, String s)
    {
        return wrapped.convertClientId(facesContext, s);
    }

    public boolean getRendersChildren()
    {
        return wrapped.getRendersChildren();
    }

    public Object getConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o)
        throws ConverterException
    {
        Object convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);

        checkProxyAlternative();

        //if the user activated the proxy mode cancel here
        if (Boolean.TRUE.equals(isAlternativeAvailable))
        {
            return convertedObject;
        }

        ValidationUtils.processExtValValidation(facesContext, uiComponent, convertedObject);

        return convertedObject;
    }

    private void checkProxyAlternative()
    {
        //to avoid a config parameter (but not nice)
        if(isAlternativeAvailable == null)
        {
            Class extValApplicationFactoryClass = ClassUtils.tryToLoadClassForName(
                "org.apache.myfaces.extensions.validator.core.proxy.ExtValApplicationFactory");

            Method isActiveMethod = ReflectionUtils.tryToGetMethod(extValApplicationFactoryClass, "isActive", null);
            isAlternativeAvailable = (Boolean)ReflectionUtils
                .tryToInvokeMethodOfClass(extValApplicationFactoryClass, isActiveMethod);

            if(isAlternativeAvailable == null)
            {
                isAlternativeAvailable = false;
            }
        }
    }
}
