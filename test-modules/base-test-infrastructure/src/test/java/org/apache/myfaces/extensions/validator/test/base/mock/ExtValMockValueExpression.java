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
package org.apache.myfaces.extensions.validator.test.base.mock;

import org.apache.myfaces.extensions.validator.core.el.ExtValELResolver;
import org.apache.myfaces.test.el.MockValueExpression;

import javax.el.ELContext;
import javax.el.ELResolver;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * @author Gerhard Petracek
 */
//TODO improve
public class ExtValMockValueExpression extends MockValueExpression
{
    private String[] elements = null;
    private String expression = null;
    private static final long serialVersionUID = -8588716545619421041L;

    public ExtValMockValueExpression(String expression, Class expectedType)
    {
        super(expression, expectedType);

        if (expression == null)
        {
            throw new NullPointerException("Expression string cannot be null");
        }

        this.expression = expression;
        parse();
    }

    @Override
    public Object getValue(ELContext context)
    {
        ELResolver resolver = context.getELResolver();

        if (resolver instanceof ExtValELResolver)
        {
            return getBase(context, resolver);
        }

        return super.getValue(context);
    }

    @Override
    public void setValue(ELContext context, Object value)
    {
        if (context.getELResolver() instanceof ExtValELResolver)
        {
            context.getELResolver().setValue(context, getValue(context), elements[elements.length - 1], null);
        }
        else
        {
            Object base = getBase(context, context.getELResolver());
            String setter = "set" + createPropertyString();

            try
            {
                Method setterMethod = base.getClass().getDeclaredMethod(setter, value != null ? value.getClass() : Object.class);
                setterMethod.invoke(base, value);
            }
            catch (Exception e)
            {
                throw new IllegalStateException();
            }
        }
    }

    @Override
    public Class getType(ELContext context)
    {
        if (context == null)
        {
            throw new NullPointerException();
        }

        Object base = getBase(context, context.getELResolver());

        if (base == null)
        {
            return null;
        }
        else
        {
            if (base instanceof Map)
            {
                return determineMapValueType(context, base);
            }

            String getter = "get" + createPropertyString();

            Method getterMethod;
            try
            {
                getterMethod = base.getClass().getDeclaredMethod(getter);
                return getterMethod.getReturnType();
            }
            catch (NoSuchMethodException e)
            {
                getter = "is" + createPropertyString();

                try
                {
                    getterMethod = base.getClass().getDeclaredMethod(getter);
                    return getterMethod.getReturnType();
                }
                catch (NoSuchMethodException e1)
                {
                    throw new IllegalStateException();
                }
            }
        }
    }

    private Class determineMapValueType(ELContext context, Object base)
    {
        Object parent = getParentOfBase(context, context.getELResolver());
        if (parent == null)
        {
            return fallbackMapValueTypeDetermination(base);
        }
        else
        {
            try
            {
                Field field = parent.getClass().getDeclaredField(elements[elements.length - 2]);
                Type type = field.getGenericType();
                if (type instanceof ParameterizedType)
                {
                    Type[] types = ((ParameterizedType) type).getActualTypeArguments();
                    return types[0].getClass();
                }
                return fallbackMapValueTypeDetermination(base);
            }
            catch (SecurityException e)
            {
                return fallbackMapValueTypeDetermination(base);
            }
            catch (NoSuchFieldException e)
            {
                return fallbackMapValueTypeDetermination(base);
            }

        }
    }

    private Class fallbackMapValueTypeDetermination(Object base)
    {
        // We can only determine the type by looking at the first value. 
        Iterator iter = ((Map) base).values().iterator();
        if (!iter.hasNext())
        {
            throw new IllegalStateException();
        }
        return iter.next().getClass();
    }

    public Object getParentOfBase(ELContext context, ELResolver resolver)
    {
        Object base = null;
        for (int i = 0; i < elements.length - 2; i++)
        {
            base = resolver.getValue(context, base, elements[i]);
        }
        return base;
    }

    private void parse()
    {

        if (isLiteralText())
        {
            elements = new String[0];
            return;
        }

        if (expression.startsWith("${") || expression.startsWith("#{"))
        {
            if (expression.endsWith("}"))
            {
                List names = new ArrayList();
                StringBuffer expr = new StringBuffer(expression.substring(2, expression.length() - 1).replaceAll(" ", ""));
                boolean isBlockOn = false;
                for (int i = expr.length() - 1; i > -1; i--)
                {
                    if (expr.charAt(i) == ' ')
                    {
                        expr.deleteCharAt(i);
                    }
                    else if (expr.charAt(i) == ']')
                    {
                        expr.deleteCharAt(i);
                    }
                    else if (expr.charAt(i) == '[')
                    {
                        expr.deleteCharAt(i);
                    }
                    else if (expr.charAt(i) == '\'')
                    {
                        if (!isBlockOn)
                        {
                            expr.deleteCharAt(i);
                        }
                        else
                        {
                            names.add(0, expr.substring(i + 1));
                            expr.delete(i, expr.length());
                        }
                        isBlockOn = !isBlockOn;
                    }
                    else if (expr.charAt(i) == '.' && !isBlockOn)
                    {
                        names.add(0, expr.substring(i + 1));
                        expr.delete(i, expr.length());
                    }
                }
                if (expr.length() > 0)
                {
                    names.add(0, expr.toString());
                }

                elements = (String[]) names.toArray(new String[names.size()]);
            }
            else
            {
                throw new IllegalArgumentException(expression);
            }
        }
        else
        {
            throw new IllegalArgumentException(expression);
        }

    }

    public Object getBase(ELContext context, ELResolver resolver)
    {
        Object base = null;
        for (int i = 0; i < elements.length - 1; i++)
        {
            base = resolver.getValue(context, base, elements[i]);
        }
        return base;
    }

    private String createPropertyString()
    {
        String property = elements[elements.length - 1];
        return property.substring(0, 1).toUpperCase() + property.substring(1, property.length());
    }
}
