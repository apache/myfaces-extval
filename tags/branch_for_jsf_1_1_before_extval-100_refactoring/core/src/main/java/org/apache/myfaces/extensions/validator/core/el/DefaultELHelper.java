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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import java.io.Externalizable;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.logging.Logger;

/**
 * in order to centralize the jsf version dependency within the core
 *
 * this el-helper supports jsp and facelets (tested with 1.1.14)
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
/*
 * there is a special facelets workaround for el-expressions of custom components
 * it's pluggable in order to support special mechanisms of different technologies (than jsp and facelets)
 * so you can plug in your own impl. which implements a custom workaround (like the facelets workaround of this impl.)
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultELHelper implements ELHelper
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    public DefaultELHelper()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    public Class getTypeOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression)
    {
        //due to a restriction with the ri
        Object bean = getValueOfExpression(facesContext, valueBindingExpression);
        return (bean != null) ? ProxyUtils.getUnproxiedClass(bean.getClass()) : null;
    }

    public Object getBean(String beanName)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        return facesContext.getApplication().getVariableResolver().resolveVariable(facesContext, beanName);
    }

    public Object getValueOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression)
    {
        return (valueBindingExpression != null) ? facesContext.getApplication()
            .createValueBinding(valueBindingExpression.getExpressionString()).getValue(facesContext) : null;
    }

    public boolean isELTermValid(FacesContext facesContext, String valueBindingExpression)
    {
        return facesContext.getApplication().createValueBinding(valueBindingExpression) != null;
    }

    private ValueBindingExpression getValueBindingExpression(UIComponent uiComponent, boolean allowBlankCharacters)
    {
        String valueBindingExpression = getOriginalValueBindingExpression(uiComponent);

        //for input components without value-binding
        //(e.g. for special component libs -> issue with ExtValRendererWrapper#encodeBegin)
        if(valueBindingExpression == null)
        {
            this.logger.finest(
                    uiComponent.getClass() + " has no value binding - component id: " + uiComponent.getId());
            return null;
        }

        if(!allowBlankCharacters)
        {
            valueBindingExpression = valueBindingExpression.replace(" ", "");
        }

        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (getTypeOfExpression(facesContext,
            new ValueBindingExpression(valueBindingExpression).getBaseExpression()) == null)
        {
            ValueBindingExpression result = FaceletsTaglibExpressionHelper.
                tryToCreateValueBindingForFaceletsBinding(uiComponent);

            if(result == null)
            {
                logger.warning("couldn't resolve expression: " + valueBindingExpression);
                return null;
            }

            Class entityClass = getTypeOfExpression(facesContext, result.getBaseExpression());

            if(entityClass == null)
            {
                logger.warning("couldn't resolve expression: " + result.getExpressionString());

                return null;
            }
            return result;
        }
        return new ValueBindingExpression(valueBindingExpression);
    }

    //keep in sync with DefaultELHelper#getPropertyDetailsViaReflectionFallback of trunk!!!
    public PropertyDetails getPropertyDetailsOfValueBinding(UIComponent uiComponent)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        ValueBindingExpression valueBindingExpression = getValueBindingExpression(uiComponent, false);

        if(valueBindingExpression == null)
        {
            return null;
        }

        ValueBindingExpression currentValueBindingExpression =
            new ValueBindingExpression(valueBindingExpression.getExpressionString());

        String path = null;

        while(currentValueBindingExpression.getBaseExpression() != null)
        {
            if(path == null)
            {
                path = getPropertyName(currentValueBindingExpression);
            }
            else
            {
                path = getPropertyName(currentValueBindingExpression) + "." + path;
            }

            currentValueBindingExpression = currentValueBindingExpression.getBaseExpression();
        }

        path = currentValueBindingExpression.getProperty() + "." + path;

        Object baseObject = getValueOfExpression(facesContext, valueBindingExpression.getBaseExpression());

        //in case of e.g.: #{bean[bean.passwordRepeatedPropertyName]}
        //-> bean.passwordRepeatedPropertyName is not the final property name
        return new PropertyDetails(path, baseObject, getPropertyName(valueBindingExpression));
    }

    private String getPropertyName(ValueBindingExpression valueBindingExpression)
    {
        String propertyName = valueBindingExpression.getProperty();

        if(propertyName.contains("."))
        {
            propertyName = extractPropertyNameOfPropertyPath(propertyName);
        }

        return propertyName;
    }

    @ToDo(value = Priority.MEDIUM, description = "support for more dynamic bindings - details see inline")
    private String extractPropertyNameOfPropertyPath(String propertyChain)
    {
        String[] properties = propertyChain.split("\\.");

        Object currentPropertyValue = getBean(properties[0]);

        Method currentMethod;
        String currentPropertyName;
        Class currentClassOfPropertyValue;
        for(int i = 1; i < properties.length; i++)
        {
            currentPropertyName = properties[i];
            currentClassOfPropertyValue = ProxyUtils.getUnproxiedClass(currentPropertyValue.getClass());
            currentMethod = ReflectionUtils.tryToGetMethod(currentClassOfPropertyValue,
                "get" + currentPropertyName.substring(0, 1).toUpperCase() + currentPropertyName.substring(1));

            if(currentMethod == null && currentPropertyValue instanceof Map)
            {
                //it's ok for the simple map case - but not for e.g.:
                //#{bean1[bean2.propertyNameProvider[ bean3.index]]}
                //or every other complex replacement for bean3.index
                //it might also require an adjustment at FaceletsTaglibExpressionHelper#tryToTransformToRealBinding
                ((Map)currentPropertyValue).get(currentPropertyName);
            }
            else
            {
                currentPropertyValue = ReflectionUtils.tryToInvokeMethod(currentPropertyValue, currentMethod);
            }
        }

        if(currentPropertyValue instanceof String)
        {
            return (String)currentPropertyValue;
        }
        else
        {
            this.logger.severe("unexpected value within map syntax: " + propertyChain +
                    " last property name: " + currentPropertyValue);
            return null;
        }
    }

    static String getOriginalValueBindingExpression(UIComponent uiComponent)
    {
        ValueBinding valueExpression = uiComponent.getValueBinding("value");

        return (valueExpression != null) ? valueExpression.getExpressionString() : null;
    }

    public boolean isELTermWellFormed(Object o)
    {
        if (o instanceof ValueBinding || o instanceof Externalizable)
        {
            return false;
        }

        String s = o.toString();
        return ((s.contains("#") || s.contains("$")) && s.contains("{") && s.contains("}"));
    }

    public Object getBindingOfComponent(UIComponent uiComponent, String name)
    {
        return uiComponent.getValueBinding(name);
    }
}
