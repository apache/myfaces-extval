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
package org.apache.myfaces.extensions.validator.core.proxy;

import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.MethodProxy;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.internal.UsageEnum;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ValidationUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import java.io.Serializable;
import java.lang.reflect.Method;

/**
 * @author Gerhard Petracek
 */
@UsageInformation({UsageEnum.ALTERNATIVE, UsageEnum.INTERNAL})
public class ExtValConverter implements Converter, MethodInterceptor,
    Serializable
{
    protected final Log logger = LogFactory.getLog(getClass());

    public static Converter newInstance(Converter wrappedConverter)
    {
        Class currentClass = wrappedConverter.getClass();

        //it's not possible to wrap the converter again - occurs e.g. under solaris + bea weblogic
        if (currentClass.getName().contains("$$EnhancerByCGLIB$$")
            || currentClass.getName().contains("$$FastClassByCGLIB$$"))
        {
            return wrappedConverter;
        }

        Enhancer enhancer = new Enhancer();
        enhancer.setSuperclass(wrappedConverter.getClass());
        enhancer.setInterfaces(new Class[]{Converter.class,
            Serializable.class});
        enhancer.setCallback(new ExtValConverter());

        ExtValUtils.increaseProcessedConverterCount();

        return (Converter) enhancer.create();
    }

    public ExtValConverter()
    {
        logger.trace("myfaces-extension-validator converter instantiated");
    }

    public Object getAsObject(FacesContext facesContext,
                              UIComponent uiComponent, String s)
    {

        Object convertedObject = getConvertedObject(facesContext, uiComponent,
            s);

        try
        {
            ValidationUtils.processExtValValidation(facesContext, uiComponent, convertedObject);
        }
        finally
        {
            //build mapping value-binding -> processed information entry
            ExtValUtils.createValueBindingConvertedValueMapping(uiComponent, convertedObject);
        }

        return convertedObject;
    }

    public String getAsString(FacesContext facesContext,
                              UIComponent uiComponent, Object o)
    {
        //indirect approach for complex components
        Converter converter = ExtValUtils.tryToCreateOriginalConverter(
            facesContext, uiComponent);
        return (converter == null) ? (o == null) ? null : o.toString()
            : converter.getAsString(facesContext, uiComponent, o);
    }

    public Object intercept(Object obj, Method method, Object[] args,
                            MethodProxy proxy) throws Throwable
    {
        Object convertedObject = proxy.invokeSuper(obj, args);

        if (method.getName().equals("getAsObject"))
        {
            try
            {
                ValidationUtils.processExtValValidation((FacesContext) args[0],
                    (UIComponent) args[1], convertedObject);
            }
            finally
            {
                //build mapping value-binding -> processed information entry
                ExtValUtils.createValueBindingConvertedValueMapping((UIComponent) args[1], convertedObject);
            }
        }
        else if (method.getName().equals("getAsString"))
        {
            storeComponentConverterMappingForProxies((FacesContext) args[0],
                (UIComponent) args[1], (Converter) obj);
        }
        return convertedObject;
    }

    protected Object getConvertedObject(FacesContext facesContext,
                                        UIComponent uiComponent, String s)
    {
        //indirect approach for complex components
        //TODO
        Converter converter = ExtValUtils.tryToCreateOriginalConverter(
            facesContext, uiComponent);
        return (converter != null) ? converter.getAsObject(facesContext,
            uiComponent, s) : s;
    }

    /*
     * private methods
     */

    private void storeComponentConverterMappingForProxies(
        FacesContext facesContext, UIComponent uiComponent,
        Converter converter)
    {
        if (ExtValUtils.useProxyMapping())
        {
            ExtValUtils.getOrInitProxyMapping().put(
                uiComponent.getClientId(facesContext), converter);
            ExtValUtils.decreaseProcessedConverterCount();
        }
    }
}
