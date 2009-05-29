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
package org.apache.myfaces.extensions.validator;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ParameterKey;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ParameterValue;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameter;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlInputSecret;
import javax.faces.component.html.HtmlSelectBooleanCheckbox;
import javax.faces.component.html.HtmlSelectOneListbox;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.component.html.HtmlSelectOneRadio;
import javax.faces.component.html.HtmlSelectManyCheckbox;
import javax.faces.component.html.HtmlSelectManyListbox;
import javax.faces.component.html.HtmlSelectManyMenu;
import javax.faces.component.html.HtmlInputTextarea;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.WildcardType;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class HtmlCoreComponentsValidationExceptionInterceptor implements ValidationExceptionInterceptor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException,
                                 ValidationStrategy validatorExceptionSource)
    {
        if(processComponent(uiComponent))
        {
            FacesContext facesContext = FacesContext.getCurrentInstance();
            FacesMessage facesMessage = ExtValUtils.convertFacesMessage(validatorException.getFacesMessage());

            String label = (String) ReflectionUtils.tryToInvokeMethod(uiComponent,
                ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "getLabel"));

            if(label == null)
            {
                label = uiComponent.getClientId(facesContext);
            }

            //override the label if the annotation provides a label
            if(metaDataEntry != null && metaDataEntry.getProperty(PropertyInformationKeys.LABEL) != null)
            {
                label = metaDataEntry.getProperty(PropertyInformationKeys.LABEL, String.class);
            }

            if(facesMessage instanceof LabeledMessage)
            {
                ((LabeledMessage)facesMessage).setLabelText(label);
            }
            //if someone uses a normal faces message
            else
            {
                for(int i = 0; i < 3; i++)
                {
                    ExtValUtils.tryToPlaceLabel(facesMessage, label, i);
                }
            }

            if(metaDataEntry != null && metaDataEntry.getValue() instanceof Annotation)
            {
                if(!displayAsException(facesMessage, metaDataEntry.getValue(Annotation.class)))
                {
                    facesContext.addMessage(uiComponent.getClientId(facesContext), facesMessage);
                    //it's a special case - since validation will continue it's essential to reset it
                    ((EditableValueHolder)uiComponent).setRequired(false);
                    return false;
                }
            }
        }
        return true;
    }

    @ToDo(value = Priority.MEDIUM, description = "refactor to a generic parameter extractor")
    private boolean displayAsException(FacesMessage facesMessage, Annotation annotation)
    {
        boolean isError = true;

        for(Method currentAnnotationAttribute : annotation.annotationType().getDeclaredMethods())
        {
            try
            {
                if(!isValidationParameter(currentAnnotationAttribute.getGenericReturnType()))
                {
                    continue;
                }

                Object parameterValue = currentAnnotationAttribute.invoke(annotation);

                if(parameterValue instanceof Class[])
                {
                    for(Class currentParameterValue : (Class[])parameterValue)
                    {
                        //keep check so that following is true:
                        //if at least one parameter is found which tells that it isn't a blocking error, let it pass
                        if(!processParameterValue(annotation, currentParameterValue, facesMessage))
                        {
                            isError = false;
                        }
                    }
                }
                else if(parameterValue instanceof Class)
                {
                    //keep check so that following is true:
                    //if at least one parameter is found which tells that it isn't a blocking error, let it pass
                    if(!processParameterValue(annotation, (Class)parameterValue, facesMessage))
                    {
                        isError = false;
                    }
                }
            }
            catch (Throwable e)
            {
                if(this.logger.isWarnEnabled())
                {
                    this.logger.warn(e);
                }
            }
        }

        return isError;
    }

    private boolean processParameterValue(Annotation annotation, Class paramClass, FacesMessage facesMessage)
            throws Exception
    {
        boolean showAsError = true;

        if(ValidationParameter.class.isAssignableFrom(paramClass))
        {
            //support pure interface approach e.g. ViolationSeverity.Warn.class
            for(Field currentField : paramClass.getDeclaredFields())
            {
                if(currentField.isAnnotationPresent(ParameterKey.class))
                {
                    Object key = currentField.get(annotation);
                    //invoke ParameterProcessors(key, annotation)
                }
                //no "else if" to allow both at one field
                if(currentField.isAnnotationPresent(ParameterValue.class))
                {
                    currentField.setAccessible(true);
                    //targetField = paramClass.getDeclaredField(currentField.getName());
                    if(!processFoundParameterValue(currentField.get(annotation), facesMessage))
                    {
                        showAsError = false;
                    }
                }
            }

            for(Class currentInterface : paramClass.getInterfaces())
            {
                if(!ValidationParameter.class.isAssignableFrom(currentInterface))
                {
                    continue;
                }

                //support interface + impl. approach e.g. MyParamImpl.class
                //(MyParamImpl implements MyParam
                //MyParam extends ValidationParameter
                //methods in the interface have to be marked with @ParameterValue and @ParameterKey
                for(Method currentMethod : currentInterface.getDeclaredMethods())
                {
                    if(currentMethod.isAnnotationPresent(ParameterKey.class))
                    {
                        Object key = currentMethod.invoke(paramClass.newInstance());
                        //invoke ParameterProcessors(key, annotation)
                    }
                    //no "else if" to allow both at one field
                    if(currentMethod.isAnnotationPresent(ParameterValue.class))
                    {
                        currentMethod.setAccessible(true);
                        if(!processFoundParameterValue(currentMethod.invoke(paramClass.newInstance()), facesMessage))
                        {
                            showAsError = false;
                        }
                    }
                }
            }
        }

        return showAsError;
    }

    private boolean processFoundParameterValue(Object value, FacesMessage facesMessage)
    {
        if(value instanceof FacesMessage.Severity)
        {
            facesMessage.setSeverity((FacesMessage.Severity)value);
            if(((FacesMessage.Severity)value).compareTo(FacesMessage.SEVERITY_ERROR) < 0)
            {
                return false;
            }
        }

        return true;
    }

    private boolean isValidationParameter(Type genericReturnType)
    {
        if(genericReturnType instanceof GenericArrayType)
        {
            if(((GenericArrayType)genericReturnType).getGenericComponentType() instanceof ParameterizedType)
            {
                return analyzeParameterizedType(
                        (ParameterizedType)((GenericArrayType)genericReturnType).getGenericComponentType());
            }
        }
        else if(genericReturnType instanceof ParameterizedType)
        {
            return analyzeParameterizedType(
                    (ParameterizedType)genericReturnType);
        }

        return false;
    }

    private boolean analyzeParameterizedType(ParameterizedType parameterizedType)
    {
        for(Type type : parameterizedType.getActualTypeArguments())
        {
            if(type instanceof WildcardType)
            {
                for(Type upperBounds : ((WildcardType)type).getUpperBounds())
                {
                    if(upperBounds instanceof Class &&
                            ((Class)upperBounds).isAssignableFrom(ValidationParameter.class))
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    protected boolean processComponent(UIComponent uiComponent)
    {
        return uiComponent instanceof HtmlInputText ||
                uiComponent instanceof HtmlInputSecret ||
                uiComponent instanceof HtmlSelectBooleanCheckbox ||
                uiComponent instanceof HtmlSelectOneListbox ||
                uiComponent instanceof HtmlSelectOneMenu ||
                uiComponent instanceof HtmlSelectOneRadio ||
                uiComponent instanceof HtmlSelectManyCheckbox ||
                uiComponent instanceof HtmlSelectManyListbox ||
                uiComponent instanceof HtmlSelectManyMenu ||
                uiComponent instanceof HtmlInputTextarea;
    }
}
