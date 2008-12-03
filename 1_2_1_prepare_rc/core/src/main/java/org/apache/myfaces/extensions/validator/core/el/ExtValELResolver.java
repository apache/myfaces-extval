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
package org.apache.myfaces.extensions.validator.core.el;

import javax.el.ELResolver;
import javax.el.ELContext;
import javax.el.VariableMapper;
import javax.el.FunctionMapper;
import java.util.Iterator;
import java.util.Locale;
import java.beans.FeatureDescriptor;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
public class ExtValELResolver extends ELResolver
{
    private ELResolver wrapped;
    private Object baseObject;
    private String property;
    //forms the id for cross-validation within complex components
    private String expression;

    public ExtValELResolver(ELResolver elResolver)
    {
        this.wrapped = elResolver;
    }

    public Object getBaseObject()
    {
        return baseObject;
    }

    public String getProperty()
    {
        return property;
    }

    public String getPath()
    {
        return expression;
    }

    public void reset()
    {
        this.baseObject = null;
        this.property = null;
        this.expression = null;
    }

    public Object getValue(ELContext elContext, Object base, Object property)
    {
        if(this.expression == null)
        {
            this.expression = (String)property;
        }
        else
        {
            this.expression += "." + property;
        }
        return wrapped.getValue(elContext, base, property);
    }

    public Class<?> getType(ELContext elContext, Object o, Object o1)
    {
        return wrapped.getType(elContext, o, o1);
    }

    public void setValue(ELContext elContext, Object o, Object o1, Object o2)
    {
        expression += "." + o1;
        property = (String)o1;
        baseObject = o;
        elContext.setPropertyResolved(true);
    }

    public boolean isReadOnly(ELContext elContext, Object o, Object o1)
    {
        return wrapped.isReadOnly(elContext, o, o1);
    }

    public Iterator<FeatureDescriptor> getFeatureDescriptors(ELContext elContext, Object o)
    {
        return wrapped.getFeatureDescriptors(elContext, o);
    }

    public Class<?> getCommonPropertyType(ELContext elContext, Object o)
    {
        return wrapped.getCommonPropertyType(elContext, o);
    }

    public static ELContext createContextWrapper(final ELContext context, final ELResolver resolver)
    {
        return new ELContext()
        {
            @Override
            public Locale getLocale()
            {
                return context.getLocale();
            }

            @Override
            public void setPropertyResolved(boolean value)
            {
                super.setPropertyResolved(value);
                context.setPropertyResolved(value);
            }

            @Override
            public void putContext(Class clazz, Object object)
            {
                super.putContext(clazz, object);
                context.putContext(clazz, object);
            }

            @Override
            public Object getContext(Class clazz)
            {
                return context.getContext(clazz);
            }

            @Override
            public void setLocale(Locale locale)
            {
                super.setLocale(locale);
                context.setLocale(locale);
            }

            @Override
            public ELResolver getELResolver()
            {
                return resolver;
            }

            @Override
            public FunctionMapper getFunctionMapper()
            {
                return context.getFunctionMapper();
            }

            @Override
            public VariableMapper getVariableMapper()
            {
                return context.getVariableMapper();
            }

        };
    }
}
